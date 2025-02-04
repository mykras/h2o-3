package hex.tree.xgboost;

import hex.*;
import hex.genmodel.MojoModel;
import hex.genmodel.MojoReaderBackend;
import hex.genmodel.MojoReaderBackendFactory;
import hex.genmodel.algos.xgboost.XGBoostMojoModel;
import hex.genmodel.algos.xgboost.XGBoostMojoReader;
import hex.genmodel.algos.xgboost.XGBoostNativeMojoModel;
import hex.genmodel.easy.EasyPredictModelWrapper;
import hex.genmodel.easy.RowData;
import hex.genmodel.easy.exception.PredictException;
import hex.genmodel.easy.prediction.BinomialModelPrediction;
import hex.genmodel.utils.DistributionFamily;
import hex.tree.xgboost.util.FeatureScore;
import ml.dmlc.xgboost4j.java.*;
import ml.dmlc.xgboost4j.java.DMatrix;
import ml.dmlc.xgboost4j.java.XGBoost;
import org.hamcrest.CoreMatchers;
import org.junit.*;
import org.junit.rules.ExpectedException;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import water.*;
import water.exceptions.H2OModelBuilderIllegalArgumentException;
import water.fvec.*;
import water.rapids.Rapids;
import water.util.ArrayUtils;
import water.util.Log;
import water.util.TwoDimTable;

import java.io.*;
import java.util.*;

import static hex.genmodel.utils.DistributionFamily.bernoulli;
import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.*;
import static water.util.FileUtils.getFile;

@RunWith(Parameterized.class)
public class XGBoostTest extends TestUtil {

  @Parameterized.Parameters(name = "XGBoost(javaMojoScoring={0},javaPredict={1}")
  public static Collection<Object[]> data() {
    return Arrays.asList(new Object[][]{
            {"false", "false"}, {"true", "true"}, {"true", "false"}, {"false", "true"}
    });
  }

  @Parameterized.Parameter
  public String confMojoJavaScoring;

  @Parameterized.Parameter(1)
  public String confJavaPredict;

  @Rule
  public transient ExpectedException thrown = ExpectedException.none();

  @Rule
  public transient TemporaryFolder tmp = new TemporaryFolder();
  
  @Before
  public void setupMojoJavaScoring() {
    System.setProperty("sys.ai.h2o.xgboost.scoring.java.enable", confMojoJavaScoring); // mojo scoring
    System.setProperty("sys.ai.h2o.xgboost.predict.java.enable", confJavaPredict); // in-h2o predict

    assertEquals(Boolean.valueOf(confMojoJavaScoring), XGBoostMojoReader.useJavaScoring(true, null)); // check that MOJO scoring config was applied
  }

  public static final class FrameMetadata {
    Vec[] vecs;
    String[] names;
    long[] checksums;
    String[][] domains;

    public FrameMetadata(Frame f) {
      vecs = f.vecs();
      names = f.names();

      checksums = new long[vecs.length];
      for (int i = 0; i < vecs.length; i++)
        checksums[i] = vecs[i].checksum();

      domains = new String[vecs.length][];
      for (int i = 0; i < vecs.length; i++)
        domains[i] = vecs[i].domain();
    }

    @Override
    public boolean equals(Object o) {
      if (! (o instanceof FrameMetadata))
        return false;

      FrameMetadata fm = (FrameMetadata)o;

      boolean error = false;

      if (vecs.length != fm.vecs.length) {
        Log.warn("Training frame vec count has changed from: " +
                vecs.length + " to: " + fm.vecs.length);
        error = true;
      }
      if (names.length != fm.names.length) {
        Log.warn("Training frame vec count has changed from: " +
                names.length + " to: " + fm.names.length);
        error = true;
      }

      for (int i = 0; i < fm.vecs.length; i++) {
        if (!fm.vecs[i].equals(fm.vecs[i])) {
          Log.warn("Training frame vec number " + i + " has changed keys.  Was: " +
                  vecs[i] + " , now: " + fm.vecs[i]);
          error = true;
        }
        if (!fm.names[i].equals(fm.names[i])) {
          Log.warn("Training frame vec number " + i + " has changed names.  Was: " +
                  names[i] + " , now: " + fm.names[i]);
          error = true;
        }
        if (checksums[i] != fm.vecs[i].checksum()) {
          Log.warn("Training frame vec number " + i + " has changed checksum.  Was: " +
                  checksums[i] + " , now: " + fm.vecs[i].checksum());
          error = true;
        }
        if (domains[i] != null && ! Arrays.equals(domains[i], fm.vecs[i].domain())) {
          Log.warn("Training frame vec number " + i + " has changed domain.  Was: " +
                  domains[i] + " , now: " + fm.vecs[i].domain());
          error = true;
        }
      }

      return !error;
    }
  }

  @BeforeClass public static void stall() {
    stall_till_cloudsize(1);

    // we need to check for XGBoost backend availability after H2O is initialized, since we
    // XGBoost is a core extension and they are registered as part of the H2O's class main method
    Assume.assumeTrue("XGBoost was not loaded!\n"
                    + "H2O XGBoost needs binary compatible environment;"
                    + "Make sure that you have correct libraries installed"
                    + "and correctly configured LD_LIBRARY_PATH, especially"
                    + "make sure that CUDA libraries are available if you are running on GPU!",
            ExtensionManager.getInstance().isCoreExtensionsEnabled(XGBoostExtension.NAME));
  }


  private static DMatrix[] getMatrices() throws XGBoostError, IOException {
    // load file from text file, also binary buffer generated by xgboost4j
    return new DMatrix[]{
            new DMatrix(getFile("smalldata/xgboost/demo/data/agaricus.txt.train").getAbsolutePath()),
            new DMatrix(getFile("smalldata/xgboost/demo/data/agaricus.txt.test").getAbsolutePath())
    };
  }
  static void saveDumpModel(File modelFile, String[] modelInfos) throws IOException {
    try{
      PrintWriter writer = new PrintWriter(modelFile, "UTF-8");
      for(int i = 0; i < modelInfos.length; ++ i) {
        writer.print("booster[" + i + "]:\n");
        writer.print(modelInfos[i]);
      }
      writer.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  static boolean checkPredicts(float[][] fPredicts, float[][] sPredicts) {
    if (fPredicts.length != sPredicts.length) {
      return false;
    }

    for (int i = 0; i < fPredicts.length; i++) {
      if (!Arrays.equals(fPredicts[i], sPredicts[i])) {
        return false;
      }
    }

    return true;
  }

  @Test
  public void testMatrices() throws XGBoostError, IOException {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    getMatrices();
    Rabit.shutdown();
  }

  @Test public void BasicModel() throws XGBoostError, IOException {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    // load file from text file, also binary buffer generated by xgboost4j
    DMatrix[] mat = getMatrices();
    DMatrix trainMat = mat[0];
    DMatrix testMat = mat[1];

    HashMap<String, Object> params = new HashMap<>();
    params.put("eta", 0.1);
    params.put("max_depth", 5);
    params.put("silent", 1);
    params.put("objective", "binary:logistic");

    HashMap<String, DMatrix> watches = new HashMap<>();
    watches.put("train", trainMat);
    watches.put("test",  testMat);

    Booster booster = XGBoost.train(trainMat, params, 10, watches, null, null);
    float[][] preds = booster.predict(trainMat, true);
    float[][] contribs = booster.predictContrib(trainMat, 0);
    for (int i = 0; i < preds.length; ++i) {
      float[] ps = preds[i];
      float[] cs = contribs[i];
      if (i < 10) {
        Log.info(ps[0] + " = Sum" + Arrays.toString(cs).replaceAll("0.0, ", ""));
      }
      assertEquals(ps[0], ArrayUtils.sum(cs), 1e-6);
    }
    Rabit.shutdown();
  }

  @Test public void testScoring() throws XGBoostError, IOException {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    // load file from text file, also binary buffer generated by xgboost4j
    DMatrix[] mat = getMatrices();
    DMatrix trainMat = mat[0];
    DMatrix testMat = mat[1];

    HashMap<String, Object> params = new HashMap<>();
    params.put("eta", 0.1);
    params.put("max_depth", 5);
    params.put("silent", 1);
    params.put("objective", "reg:linear");

    HashMap<String, DMatrix> watches = new HashMap<>();
    watches.put("train", trainMat);
    watches.put("test",  testMat);

    Booster booster = XGBoost.train(trainMat, params, 10, watches, null, null);
    // slice some rows out and predict on those
    float[][] preds1 = booster.predict(trainMat.slice(new int[]{0}));
    float[][] preds2 = booster.predict(trainMat.slice(new int[]{1}));
    float[][] preds3 = booster.predict(trainMat.slice(new int[]{2}));
    float[][] preds4 = booster.predict(trainMat.slice(new int[]{0,1,2}));

    assertEquals(preds1.length, 1);
    assertEquals(1, preds2.length);
    assertEquals(1, preds3.length);
    assertEquals(3, preds4.length);

    assertEquals(preds4[0][0], preds1[0][0], 1e-10);
    assertEquals(preds4[1][0], preds2[0][0], 1e-10);
    assertEquals(preds4[2][0], preds3[0][0], 1e-10);
    assertNotEquals(preds4[0][0], preds4[1][0], 1e-10);
    assertNotEquals(preds4[0][0],preds4[2][0], 1e-10);
    Rabit.shutdown();
  }

  @Test public void testScore0() throws XGBoostError {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    // trivial dataset with 3 rows and 2 columns
    // (4,5) -> 1
    // (3,1) -> 2
    // (2,3) -> 3
    DMatrix trainMat = new DMatrix(new float[]{4f,5f, 3f,1f, 2f,3f},3,2);
    trainMat.setLabel(new float[]{             1f,    2f,    3f       });

    HashMap<String, Object> params = new HashMap<>();
    params.put("eta", 1);
    params.put("max_depth", 5);
    params.put("silent", 1);
    params.put("objective", "reg:linear");

    HashMap<String, DMatrix> watches = new HashMap<>();
    watches.put("train", trainMat);

    Booster booster = XGBoost.train(trainMat, params, 10, watches, null, null);

    // check overfitting
    // (4,5) -> 1
    // (3,1) -> 2
    // (2,3) -> 3
    float[][] preds1 = booster.predict(new DMatrix(new float[]{4f,5f},1,2));
    float[][] preds2 = booster.predict(new DMatrix(new float[]{3f,1f},1,2));
    float[][] preds3 = booster.predict(new DMatrix(new float[]{2f,3f},1,2));

    assertEquals(preds1.length, 1);
    assertEquals(preds2.length, 1);
    assertEquals(preds3.length, 1);

    assertTrue(Math.abs(preds1[0][0]-1) < 1e-2);
    assertTrue(Math.abs(preds2[0][0]-2) < 1e-2);
    assertTrue(Math.abs(preds3[0][0]-3) < 1e-2);
    Rabit.shutdown();
  }

  @Test public void testExtendedFeatureScore() throws XGBoostError, IOException {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    // load file from text file, also binary buffer generated by xgboost4j
    DMatrix[] mat = getMatrices();
    DMatrix trainMat = mat[0];
    DMatrix testMat = mat[1];

    HashMap<String, Object> params = new HashMap<>();
    params.put("eta", 0.1);
    params.put("max_depth", 5);
    params.put("silent", 1);
    params.put("objective", "reg:linear");
    params.put("seed", 12);

    HashMap<String, DMatrix> watches = new HashMap<>();
    watches.put("train", trainMat);
    watches.put("test",  testMat);

    Booster booster = XGBoost.train(trainMat, params, 10, watches, null, null);

    final Map<String, Integer> expected = booster.getFeatureScore((String) null);
    final Map<String, FeatureScore> actual = getExtFeatureScore(booster);

    assertEquals(expected.keySet(), actual.keySet());
    for (String feature : expected.keySet()) {
      assertEquals((int) expected.get(feature), actual.get(feature)._frequency);
    }

    Booster booster1 = XGBoost.train(trainMat, params, 1, watches, null, null);
    Booster booster2 = XGBoost.train(trainMat, params, 2, watches, null, null);

    // Check that gain(booster2) >= gain(booster1)
    final Map<String, FeatureScore> fs1 = getExtFeatureScore(booster1);
    final Map<String, FeatureScore> fs2 = getExtFeatureScore(booster2);

    for (String feature : fs2.keySet()) {
      assertTrue(fs2.get(feature)._gain > 0);
      assertTrue(fs1.get(feature)._gain <= fs2.get(feature)._gain);
    }
    
    Rabit.shutdown();
  }

  private Map<String, FeatureScore> getExtFeatureScore(Booster booster) throws XGBoostError {
    String[] modelDump = booster.getModelDump((String) null, true);
    return XGBoostUtils.parseFeatureScores(modelDump);
  }

  @Test
  public void saveLoadDataAndModel() throws XGBoostError, IOException {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    // load file from text file, also binary buffer generated by xgboost4j
    DMatrix[] mat = getMatrices();
    DMatrix trainMat = mat[0];
    DMatrix testMat = mat[1];

    HashMap<String, Object> params = new HashMap<>();
    params.put("eta", 0.1);
    params.put("max_depth", 5);
    params.put("silent", 1);
    params.put("objective", "binary:logistic");

    HashMap<String, DMatrix> watches = new HashMap<>();
    watches.put("train", trainMat);
    watches.put("test",  testMat);

    Booster booster = XGBoost.train(trainMat, params, 10, watches, null, null);

    float[][] predicts = booster.predict(testMat);

    //save model to modelPath
    File modelDir = java.nio.file.Files.createTempDirectory("xgboost-model").toFile();

    booster.saveModel(path(modelDir, "xgb.model"));

    //dump model with feature map
    String[] modelInfos = booster.getModelDump(getFile("smalldata/xgboost/demo/data/featmap.txt").getAbsolutePath(), false);
    saveDumpModel(new File(modelDir, "dump.raw.txt"), modelInfos);

    //save dmatrix into binary buffer
    testMat.saveBinary(path(modelDir, "dtest.buffer"));

    //reload model and data
    Booster booster2 = XGBoost.loadModel(path(modelDir, "xgb.model"));
    DMatrix testMat2 = new DMatrix(path(modelDir, "dtest.buffer"));
    float[][] predicts2 = booster2.predict(testMat2);

    //check the two predicts
    System.out.println(checkPredicts(predicts, predicts2));

    //specify watchList
    HashMap<String, DMatrix> watches2 = new HashMap<>();
    watches2.put("train", trainMat);
    watches2.put("test", testMat2);
    Booster booster3 = XGBoost.train(trainMat, params, 10, watches2, null, null);
    float[][] predicts3 = booster3.predict(testMat2);

    //check predicts
    System.out.println(checkPredicts(predicts, predicts3));
    Rabit.shutdown();
  }

  private static String path(File parentDir, String fileName) {
    return new File(parentDir, fileName).getAbsolutePath();
  }

  @Test
  public void checkpoint() throws XGBoostError, IOException {
    Map<String, String> rabitEnv = new HashMap<>();
    rabitEnv.put("DMLC_TASK_ID", "0");
    Rabit.init(rabitEnv);
    // load file from text file, also binary buffer generated by xgboost4j
    DMatrix[] mat = getMatrices();
    DMatrix trainMat = mat[0];
    DMatrix testMat = mat[1];

    HashMap<String, Object> params = new HashMap<>();
    params.put("eta", 0.1);
    params.put("max_depth", 5);
    params.put("silent", 1);
    params.put("objective", "binary:logistic");

    HashMap<String, DMatrix> watches = new HashMap<>();
    watches.put("train", trainMat);

    Booster booster = XGBoost.train(trainMat, params, 0, watches, null, null);
    // Train for 10 iterations
    for (int i=0;i<10;++i) {
      booster.update(trainMat, i);
      float[][] preds = booster.predict(testMat);
      for (int j = 0; j < 10; ++j)
        Log.info(preds[j][0]);
    }
    Rabit.shutdown();
  }

  @Test
  public void WeatherBinary() {
    Frame tfr = null;
    Frame trainFrame = null;
    Frame testFrame = null;
    Frame preds = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      // Parse frame into H2O
      tfr = parse_test_file("./smalldata/junit/weather.csv");
      // define special columns
      String response = "RainTomorrow";
//      String weight = null;
//      String fold = null;
      Scope.track(tfr.replace(tfr.find(response), tfr.vecs()[tfr.find(response)].toCategoricalVec()));
      // remove columns correlated with the response
      tfr.remove("RISK_MM").remove();
      tfr.remove("EvapMM").remove();
      FrameMetadata metadataBefore = new FrameMetadata(tfr);  // make sure it's after removing those columns!
      DKV.put(tfr);

      // split into train/test
      SplitFrame sf = new SplitFrame(tfr, new double[] { 0.7, 0.3 }, null);
      sf.exec().get();
      Key[] ksplits = sf._destination_frames;
      trainFrame = (Frame)ksplits[0].get();
      testFrame = (Frame)ksplits[1].get();

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 5;
      parms._max_depth = 5;
      parms._train = trainFrame._key;
      parms._valid = testFrame._key;
      parms._response_column = response;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(testFrame);
      assertTrue(model.testJavaScoring(testFrame, preds, 1e-6));
      assertEquals(
              ((ModelMetricsBinomial)model._output._validation_metrics).auc(),
              ModelMetricsBinomial.make(preds.vec(2), testFrame.vec(response)).auc(),
              1e-5
      );
      assertTrue(preds.anyVec().sigma() > 0);

    } finally {
      Scope.exit();
      if (trainFrame!=null) trainFrame.remove();
      if (testFrame!=null) testFrame.remove();
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) model.delete();
    }
  }

  @Test
  public void WeatherBinaryCV() {
    Frame tfr = null;
    Frame trainFrame = null;
    Frame testFrame = null;
    Frame preds = null;
    XGBoostModel model = null;
    try {
      Scope.enter();
      // Parse frame into H2O
      tfr = parse_test_file("./smalldata/junit/weather.csv");
      // define special columns
      String response = "RainTomorrow";
//      String weight = null;
//      String fold = null;
      Scope.track(tfr.replace(tfr.find(response), tfr.vecs()[tfr.find(response)].toCategoricalVec()));
      // remove columns correlated with the response
      tfr.remove("RISK_MM").remove();
      tfr.remove("EvapMM").remove();
      FrameMetadata metadataBefore = new FrameMetadata(tfr);  // make sure it's after removing those columns!
      DKV.put(tfr);

      // split into train/test
      SplitFrame sf = new SplitFrame(tfr, new double[] { 0.7, 0.3 }, null);
      sf.exec().get();
      Key[] ksplits = sf._destination_frames;
      trainFrame = (Frame)ksplits[0].get();
      testFrame = (Frame)ksplits[1].get();


      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 5;
      parms._max_depth = 5;
      parms._train = trainFrame._key;
      parms._valid = testFrame._key;
      parms._nfolds = 5;
      parms._response_column = response;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(testFrame);
      assertTrue(model.testJavaScoring(testFrame, preds, 1e-6));
      assertEquals(
              ((ModelMetricsBinomial)model._output._validation_metrics).auc(),
              ModelMetricsBinomial.make(preds.vec(2), testFrame.vec(response)).auc(),
              1e-5
      );
      assertTrue(preds.anyVec().sigma() > 0);

    } finally {
      Scope.exit();
      if (trainFrame!=null) trainFrame.remove();
      if (testFrame!=null) testFrame.remove();
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) {
        model.deleteCrossValidationModels();
        model.delete();
      }
    }
  }

  @Test
  public void testWeatherBinaryCVEarlyStopping() {
    XGBoostModel model = null;
    try {
      Scope.enter();
      Frame tfr = Scope.track(parse_test_file("./smalldata/junit/weather.csv"));
      final String response = "RainTomorrow";
      Scope.track(tfr.replace(tfr.find(response), tfr.vecs()[tfr.find(response)].toCategoricalVec()));
      // remove columns correlated with the response
      tfr.remove("RISK_MM").remove();
      tfr.remove("EvapMM").remove();
      DKV.put(tfr);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 50;
      parms._max_depth = 5;
      parms._train = tfr._key;
      parms._nfolds = 5;
      parms._response_column = response;
      parms._stopping_rounds = 3;
      parms._score_tree_interval = 1;
      parms._seed = 123;
      parms._keep_cross_validation_models = true;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      final int ntrees = model._output._ntrees;

      int expected = 0;
      for (Key k : model._output._cross_validation_models) {
        expected += ((XGBoostModel) k.get())._output._ntrees;
      }
      expected = (int) ((double) expected) / model._output._cross_validation_models.length;

      assertEquals(expected, ntrees);
    } finally {
      Scope.exit();
      if (model!=null) {
        model.deleteCrossValidationModels();
        model.delete();
      }
    }
  }

  @Test(expected = H2OModelBuilderIllegalArgumentException.class)
  public void RegressionCars() {
    Frame tfr = null;
    Frame trainFrame = null;
    Frame testFrame = null;
    Frame preds = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      // Parse frame into H2O
      tfr = parse_test_file("./smalldata/junit/cars.csv");
      FrameMetadata metadataBefore = new FrameMetadata(tfr);
      DKV.put(tfr);

      Scope.track(tfr.replace(1, tfr.vecs()[1].toCategoricalVec()));   // Convert CAPSULE to categorical
      Scope.track(tfr.replace(3, tfr.vecs()[3].toCategoricalVec()));   // Convert RACE to categorical
      DKV.put(tfr);

      // split into train/test
      SplitFrame sf = new SplitFrame(tfr, new double[] { 0.7, 0.3 }, null);
      sf.exec().get();
      Key[] ksplits = sf._destination_frames;
      trainFrame = (Frame)ksplits[0].get();
      testFrame = (Frame)ksplits[1].get();

      // define special columns
//      String response = "cylinders"; // passes
      String response = "economy (mpg)"; //Expected to fail - contains NAs

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = trainFrame._key;
      parms._valid = testFrame._key;
      parms._response_column = response;
      parms._ignored_columns = new String[]{"name"};

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(testFrame);
      assertTrue(model.testJavaScoring(testFrame, preds, 1e-6));
      assertEquals(
              ((ModelMetricsRegression)model._output._validation_metrics).mae(),
              ModelMetricsRegression.make(preds.anyVec(), testFrame.vec(response), DistributionFamily.gaussian).mae(),
              1e-5
      );
      assertTrue(preds.anyVec().sigma() > 0);

    } finally {
      Scope.exit();
      if (trainFrame!=null) trainFrame.remove();
      if (testFrame!=null) testFrame.remove();
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) {
        model.delete();
      }
    }
  }

  @Test
  public void ProstateRegression() {
    Frame tfr = null;
    Frame trainFrame = null;
    Frame testFrame = null;
    Frame preds = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      // Parse frame into H2O
      tfr = parse_test_file("./smalldata/prostate/prostate.csv");
      FrameMetadata metadataBefore = new FrameMetadata(tfr);

      Scope.track(tfr.replace(1, tfr.vecs()[1].toCategoricalVec()));   // Convert CAPSULE to categorical
      Scope.track(tfr.replace(3, tfr.vecs()[3].toCategoricalVec()));   // Convert RACE to categorical
      DKV.put(tfr);

      // split into train/test
      SplitFrame sf = new SplitFrame(tfr, new double[] { 0.7, 0.3 }, null);
      sf.exec().get();
      Key[] ksplits = sf._destination_frames;
      trainFrame = (Frame)ksplits[0].get();
      testFrame = (Frame)ksplits[1].get();

      // define special columns
      String response = "AGE";
//      String weight = null;
//      String fold = null;

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = trainFrame._key;
      parms._valid = testFrame._key;
      parms._response_column = response;
      parms._ignored_columns = new String[]{"ID"};

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(testFrame);
      assertTrue(model.testJavaScoring(testFrame, preds, 1e-6));
      assertEquals(
              ((ModelMetricsRegression)model._output._validation_metrics).mae(),
              ModelMetricsRegression.make(preds.anyVec(), testFrame.vec(response), DistributionFamily.gaussian).mae(),
              1e-5
      );
      assertTrue(preds.anyVec().sigma() > 0);
    } finally {
      Scope.exit();
      if (trainFrame!=null) trainFrame.remove();
      if (testFrame!=null) testFrame.remove();
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) {
        model.delete();
      }
    }
  }

  @Test
  public void sparseMatrixDetectionTest() {
    Frame tfr = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      tfr = parse_test_file("./smalldata/prostate/prostate.csv");
      Scope.track(tfr.replace(8, tfr.vecs()[8].toCategoricalVec()));   // Convert GLEASON to categorical
      DKV.put(tfr);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      // Automatic detection should compute sparsity and decide
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = "AGE";
      parms._train = tfr._key;
      parms._ignored_columns = new String[]{"ID","DPROS", "DCAPS", "PSA", "VOL", "RACE", "CAPSULE"};

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      assertTrue(model._output._sparse);

    } finally {
      Scope.exit();
      if (tfr!=null) tfr.remove();
      if (model!=null) {
        model.delete();
        model.deleteCrossValidationModels();
      }
    }

  }

  @Test
  public void testNativeParams() {
    Frame tfr = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      tfr = parse_test_file("./smalldata/prostate/prostate.csv");

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = tfr._key;
      parms._response_column = "AGE";
      parms._ignored_columns = new String[]{"ID"};
      parms._backend = XGBoostModel.XGBoostParameters.Backend.cpu;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();

      TwoDimTable np = model._output._native_parameters;
      assertNotNull(np);
      System.out.println(np.toString());

      Set<String> names = new HashSet<>();
      for (int i = 0; i < np.getRowDim(); i++) {
        names.add(String.valueOf(np.get(i, 0)));
      }
      assertEquals(names, new HashSet<>(Arrays.asList(
              "colsample_bytree", "silent", "tree_method", "seed", "max_depth", "booster", "objective", "nround",
              "lambda", "eta", "grow_policy", "nthread", "alpha", "colsample_bylevel", "subsample", "min_child_weight",
              "gamma", "max_delta_step")));
    } finally {
      Scope.exit();
      if (tfr!=null) tfr.remove();
      if (model!=null) {
        model.delete();
        model.deleteCrossValidationModels();
      }
    }
  }

  @Test
  public void testBinomialTrainingWeights() {
    XGBoostModel model = null;
    XGBoostModel noWeightsModel = null;
    Scope.enter();
    try {
      Frame airlinesFrame = Scope.track(parse_test_file("./smalldata/testng/airlines.csv"));
      airlinesFrame.replace(0, airlinesFrame.vecs()[0].toCategoricalVec()).remove();

      final Vec weightsVector = createRandomBinaryWeightsVec(airlinesFrame.numRows(), 10);
      final String weightsColumnName = "weights";
      airlinesFrame.add(weightsColumnName, weightsVector);
      DKV.put(airlinesFrame);


      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = "IsDepDelayed";
      parms._train = airlinesFrame._key;
      parms._backend = XGBoostModel.XGBoostParameters.Backend.cpu;
      parms._weights_column = weightsColumnName;
      parms._ignored_columns = new String[]{"fYear", "fMonth", "fDayofMonth", "fDayOfWeek"};

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      assertEquals(ModelCategory.Binomial, model._output.getModelCategory());
      assertEquals(model._output.weightsName(), weightsColumnName);

      Frame trainingFrameSubset = Rapids.exec(String.format("(rows %s ( == (cols %s [9]) 1))", airlinesFrame._key, airlinesFrame._key)).getFrame();
      trainingFrameSubset._key = Key.make();
      Scope.track(trainingFrameSubset);
      assertEquals(airlinesFrame.vec(weightsColumnName).nzCnt(), trainingFrameSubset.numRows());
      DKV.put(trainingFrameSubset);
      parms._weights_column = null;
      parms._train = trainingFrameSubset._key;

      noWeightsModel = new hex.tree.xgboost.XGBoost(parms).trainModel().get();

      Vec predicted = Scope.track(noWeightsModel.score(trainingFrameSubset)).vec(2);
      ModelMetricsBinomial expected = ModelMetricsBinomial.make(predicted, trainingFrameSubset.vec("IsDepDelayed"));

      checkMetrics(expected, (ModelMetricsBinomial) noWeightsModel._output._training_metrics);
      checkMetrics(expected, (ModelMetricsBinomial) model._output._training_metrics);
    } finally {
      Scope.exit();
      if (model != null) model.delete();
      if (noWeightsModel != null) noWeightsModel.delete();
    }

  }

  @Test
  public void testRegressionTrainingWeights() {
    XGBoostModel model = null;
    XGBoostModel noWeightsModel = null;
    Scope.enter();
    try {
      Frame prostateFrame = parse_test_file("./smalldata/prostate/prostate.csv");
      prostateFrame.replace(8, prostateFrame.vecs()[8].toCategoricalVec()).remove();   // Convert GLEASON to categorical
      Scope.track(prostateFrame);

      final Vec weightsVector = createRandomBinaryWeightsVec(prostateFrame.numRows(), 10);
      final String weightsColumnName = "weights";
      prostateFrame.add(weightsColumnName, weightsVector);
      DKV.put(prostateFrame);


      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = "AGE";
      parms._train = prostateFrame._key;
      parms._weights_column = weightsColumnName;
      parms._ignored_columns = new String[]{"ID"};

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      assertEquals(ModelCategory.Regression, model._output.getModelCategory());
      assertEquals(weightsColumnName, model._output.weightsName());

      Frame trainingFrameSubset = Rapids.exec(String.format("(rows %s ( == (cols %s [9]) 1))", prostateFrame._key, prostateFrame._key)).getFrame();
      trainingFrameSubset._key = Key.make();
      Scope.track(trainingFrameSubset);
      assertEquals(prostateFrame.vec(weightsColumnName).nzCnt(), trainingFrameSubset.numRows());
      DKV.put(trainingFrameSubset);
      parms._weights_column = null;
      parms._train = trainingFrameSubset._key;

      noWeightsModel = new hex.tree.xgboost.XGBoost(parms).trainModel().get();

      Vec predicted = Scope.track(noWeightsModel.score(trainingFrameSubset)).vec(0);
      ModelMetricsRegression expected = ModelMetricsRegression.make(predicted, trainingFrameSubset.vec("AGE"), DistributionFamily.gaussian);

      checkMetrics(expected, (ModelMetricsRegression) noWeightsModel._output._training_metrics);
      checkMetrics(expected, (ModelMetricsRegression) model._output._training_metrics);
    } finally {
      Scope.exit();
      if (model != null) model.delete();
      if (noWeightsModel != null) noWeightsModel.delete();
    }

  }

  @Test
  public void testMultinomialTrainingWeights() {
    XGBoostModel model = null;
    XGBoostModel noWeightsModel = null;
    Scope.enter();
    try {
      Frame irisFrame = parse_test_file("./smalldata/extdata/iris.csv");
      irisFrame.replace(4, irisFrame.vecs()[4].toCategoricalVec()).remove();
      Scope.track(irisFrame);

      final Vec weightsVector = createRandomBinaryWeightsVec(irisFrame.numRows(), 10);
      final String weightsColumnName = "weights";
      irisFrame.add(weightsColumnName, weightsVector);
      DKV.put(irisFrame);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = "C5"; // iris-setosa, iris-versicolor, iris-virginica
      parms._train = irisFrame._key;
      parms._weights_column = weightsColumnName;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      assertEquals(ModelCategory.Multinomial, model._output.getModelCategory());
      assertEquals(weightsColumnName, model._output.weightsName());

      Frame trainingFrameSubset = Rapids.exec(String.format("(rows %s ( == (cols %s [5]) 1))", irisFrame._key, irisFrame._key)).getFrame();
      trainingFrameSubset._key = Key.make();
      Scope.track(trainingFrameSubset);
      assertEquals(irisFrame.vec(weightsColumnName).nzCnt(), trainingFrameSubset.numRows());
      DKV.put(trainingFrameSubset);
      parms._weights_column = null;
      parms._train = trainingFrameSubset._key;

      noWeightsModel = new hex.tree.xgboost.XGBoost(parms).trainModel().get();

      Frame predicted = Scope.track(noWeightsModel.score(trainingFrameSubset));
      predicted.remove(0);
      ModelMetricsMultinomial expected = ModelMetricsMultinomial.make(predicted, trainingFrameSubset.vec("C5"));

      checkMetrics(expected, (ModelMetricsMultinomial) model._output._training_metrics);
      checkMetrics(expected, (ModelMetricsMultinomial) noWeightsModel._output._training_metrics);
    } finally {
      Scope.exit();
      if (model != null) model.delete();
      if (noWeightsModel != null) noWeightsModel.delete();
    }

  }

  /**
   * @param len        Length of the resulting vector
   * @param randomSeed Seed for the random generator (for reproducibility)
   * @return An instance of {@link Vec} with binary weights (either 0.0D or 1.0D, nothing in between).
   */
  private Vec createRandomBinaryWeightsVec(final long len, final int randomSeed) {
    final Vec weightsVec = Vec.makeZero(len, Vec.T_NUM);
    final Random random = new Random(randomSeed);
    for (int i = 0; i < weightsVec.length(); i++) {
      weightsVec.set(i, random.nextBoolean() ? 1.0D : 0D);
    }

    return weightsVec;
  }

  @Test
  public void denseMatrixDetectionTest() {
    Frame tfr = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      tfr = parse_test_file("./smalldata/prostate/prostate.csv");
      DKV.put(tfr);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      // Automatic detection should compute sparsity and decide
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = "AGE";
      parms._train = tfr._key;
      parms._ignored_columns = new String[]{"ID","DPROS", "DCAPS", "PSA", "VOL", "RACE", "CAPSULE"};

      // GLEASON used as predictor variable, numeric variable, dense
      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      assertFalse(model._output._sparse);

    } finally {
      Scope.exit();
      if (tfr!=null) tfr.remove();
      if (model!=null) {
        model.delete();
        model.deleteCrossValidationModels();
      }
    }

  }

  @Test
  public void ProstateRegressionCV() {
    for (XGBoostModel.XGBoostParameters.DMatrixType dMatrixType : XGBoostModel.XGBoostParameters.DMatrixType.values()) {
      Frame tfr = null;
      Frame trainFrame = null;
      Frame testFrame = null;
      Frame preds = null;
      XGBoostModel model = null;
      try {
        // Parse frame into H2O
        tfr = parse_test_file("./smalldata/prostate/prostate.csv");
        FrameMetadata metadataBefore = new FrameMetadata(tfr);

        // split into train/test
        SplitFrame sf = new SplitFrame(tfr, new double[] { 0.7, 0.3 }, null);
        sf.exec().get();
        Key[] ksplits = sf._destination_frames;
        trainFrame = (Frame)ksplits[0].get();
        testFrame = (Frame)ksplits[1].get();

        // define special columns
        String response = "AGE";
//      String weight = null;
//      String fold = null;

        XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
        parms._dmatrix_type = dMatrixType;
        parms._nfolds = 2;
        parms._train = trainFrame._key;
        parms._valid = testFrame._key;
        parms._response_column = response;
        parms._ignored_columns = new String[]{"ID"};

        model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
        Log.info(model);

        FrameMetadata metadataAfter = new FrameMetadata(tfr);
        assertEquals(metadataBefore, metadataAfter);

        preds = model.score(testFrame);
        assertTrue(model.testJavaScoring(testFrame, preds, 1e-6));
        assertTrue(preds.anyVec().sigma() > 0);

      } finally {
        if (trainFrame!=null) trainFrame.remove();
        if (testFrame!=null) testFrame.remove();
        if (tfr!=null) tfr.remove();
        if (preds!=null) preds.remove();
        if (model!=null) {
          model.delete();
          model.deleteCrossValidationModels();
        }
      }
    }
  }

  @Test
  public void MNIST() {
    Frame tfr = null;
    Frame preds = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      // Parse frame into H2O
      tfr = parse_test_file("bigdata/laptop/mnist/train.csv.gz");
      FrameMetadata metadataBefore = new FrameMetadata(tfr);
      Scope.track(tfr.replace(784, tfr.vecs()[784].toCategoricalVec()));   // Convert response 'C785' to categorical
      DKV.put(tfr);

      // define special columns
      String response = "C785";

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 3;
      parms._max_depth = 3;
      parms._train = tfr._key;
      parms._response_column = response;
      parms._seed = 0xCAFEBABE;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(tfr);
      assertTrue(model.testJavaScoring(tfr, preds, 1e-6));
      preds.remove(0).remove();
      assertTrue(preds.anyVec().sigma() > 0);
      assertEquals(
              ((ModelMetricsMultinomial)model._output._training_metrics).logloss(),
              ModelMetricsMultinomial.make(preds, tfr.vec(response), tfr.vec(response).domain()).logloss(),
              1e-5
      );
    } finally {
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) model.delete();
      Scope.exit();
    }
  }

  @Test
  public void testGPUIncompatParams() {
    XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
    parms._backend = XGBoostModel.XGBoostParameters.Backend.gpu;
    parms._grow_policy = XGBoostModel.XGBoostParameters.GrowPolicy.lossguide;
    Map<String, Object> expectedIncompats = Collections.singletonMap("grow_policy", (Object) XGBoostModel.XGBoostParameters.GrowPolicy.lossguide);
    assertEquals(expectedIncompats, parms.gpuIncompatibleParams());
  }

  @Test
  public void testGPUIncompats() {
    Scope.enter();
    try {
      Frame tfr = new TestFrameBuilder()
              .withName("testFrame")
              .withColNames("ColA", "ColB")
              .withVecTypes(Vec.T_NUM, Vec.T_CAT)
              .withDataForCol(0, ard(Double.NaN, 1, 2, 3, 4, 5.6, 7))
              .withDataForCol(1, ar("A", "B,", "A", "C", "A", "B", "A"))
              .build();
      Scope.track(tfr);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 3;
      parms._max_depth = 3;
      parms._train = tfr._key;
      parms._response_column = "ColB";

      // Force GPU backend
      parms._backend = XGBoostModel.XGBoostParameters.Backend.gpu;

      // Set GPU incompatible parameter 'grow_policy = lossguide'
      parms._grow_policy = XGBoostModel.XGBoostParameters.GrowPolicy.lossguide;
      parms._tree_method = XGBoostModel.XGBoostParameters.TreeMethod.hist; // Needed by lossguide

      try {
        XGBoostModel model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
        Scope.track_generic(model);
        fail("Thes parameter settings are not suppose to work!");
      } catch (H2OModelBuilderIllegalArgumentException e) {
        String expected = "ERRR on field: _backend: GPU backend is not available for parameter setting 'grow_policy = lossguide'. Use CPU backend instead.\n";
        assertTrue(e.getMessage().endsWith(expected));
      }
    } finally {
      Scope.exit();
    }
  }

  @Test
  public void MNIST_LightGBM() {
    Frame tfr = null;
    Frame preds = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      // Parse frame into H2O
      tfr = parse_test_file("bigdata/laptop/mnist/train.csv.gz");
      FrameMetadata metadataBefore = new FrameMetadata(tfr);
      Scope.track(tfr.replace(784, tfr.vecs()[784].toCategoricalVec()));   // Convert response 'C785' to categorical
      DKV.put(tfr);

      // define special columns
      String response = "C785";

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 3;
      parms._max_depth = 3;
      parms._train = tfr._key;
      parms._response_column = response;
      parms._seed = 0xCAFEBABE;

      // emulate LightGBM
      parms._tree_method = XGBoostModel.XGBoostParameters.TreeMethod.hist;
      parms._grow_policy = XGBoostModel.XGBoostParameters.GrowPolicy.lossguide;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(tfr);
      assertTrue(model.testJavaScoring(tfr, preds, 1e-6));
      preds.remove(0).remove();
      assertTrue(preds.anyVec().sigma() > 0);
      assertEquals(
              ((ModelMetricsMultinomial)model._output._training_metrics).logloss(),
              ModelMetricsMultinomial.make(preds, tfr.vec(response), tfr.vec(response).domain()).logloss(),
              1e-5
      );
    } finally {
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) model.delete();
      Scope.exit();
    }
  }

  @Ignore
  @Test
  public void testCSC() {
    Frame tfr = null;
    Frame preds = null;
    XGBoostModel model = null;
    Scope.enter();
    try {
      // Parse frame into H2O
      tfr = parse_test_file("csc.csv");
      FrameMetadata metadataBefore = new FrameMetadata(tfr);
      String response = "response";

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 3;
      parms._max_depth = 3;
      parms._train = tfr._key;
      parms._response_column = response;

      model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Log.info(model);

      FrameMetadata metadataAfter = new FrameMetadata(tfr);
      assertEquals(metadataBefore, metadataAfter);

      preds = model.score(tfr);
      assertTrue(model.testJavaScoring(tfr, preds, 1e-6));
      assertTrue(preds.vec(2).sigma() > 0);
      assertEquals(
              ((ModelMetricsBinomial)model._output._training_metrics).logloss(),
              ModelMetricsBinomial.make(preds.vec(2), tfr.vec(response), tfr.vec(response).domain()).logloss(),
              1e-5
      );
    } finally {
      if (tfr!=null) tfr.remove();
      if (preds!=null) preds.remove();
      if (model!=null) model.delete();
      Scope.exit();
    }
  }

  @Test
  public void testModelMetrics() {
      Frame tfr = null, trainFrame = null, testFrame = null, validFrame = null;
      XGBoostModel model = null;
      try {
        // Parse frame into H2O
        tfr = parse_test_file("./smalldata/prostate/prostate.csv");
        FrameMetadata metadataBefore = new FrameMetadata(tfr);

        // split into train/test
        SplitFrame sf = new SplitFrame(tfr, new double[] { 0.6, 0.2, 0.2 }, null);
        sf.exec().get();

        trainFrame = sf._destination_frames[0].get();
        testFrame = sf._destination_frames[1].get();
        validFrame = sf._destination_frames[2].get();
        String response = "AGE";

        XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
        parms._ntrees = 2;
        parms._train = trainFrame._key;
        parms._valid = testFrame._key;
        parms._response_column = response;
        parms._ignored_columns = new String[]{"ID"};

        model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
        assertNotNull("Train metrics are not null", model._output._training_metrics);
        assertNotNull("Validation metrics are not null", model._output._validation_metrics);
        assertEquals("Initial model output metrics contains 2 model metrics",
                            2, model._output.getModelMetrics().length);
        for(String name : model._output._names){
          assertNotEquals(parms._ignored_columns[0], name);
        }

        model.score(testFrame).remove();
        assertEquals("After scoring on test data, model output metrics contains 2 model metrics",
                            2, model._output.getModelMetrics().length);

        model.score(validFrame).remove();
        assertEquals("After scoring on unseen data, model output metrics contains 3 model metrics",
                            3, model._output.getModelMetrics().length);


        FrameMetadata metadataAfter = new FrameMetadata(tfr);
        assertEquals(metadataBefore, metadataAfter);

      } finally {
        if (trainFrame!=null) trainFrame.remove();
        if (testFrame!=null) testFrame.remove();
        if (validFrame!=null) validFrame.remove();
        if (tfr!=null) tfr.remove();
        if (model!=null) {
          model.delete();
        }
      }
  }

  @Test
  public void testCrossValidation() {
    Scope.enter();
    XGBoostModel denseModel = null;
    XGBoostModel sparseModel = null;
    try {
      Frame tfr = Scope.track(parse_test_file("./smalldata/prostate/prostate.csv"));

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = tfr._key;
      parms._response_column = "AGE";
      parms._ignored_columns = new String[]{"ID"};
      parms._seed = 42;
      parms._ntrees = 5;
      parms._weights_column = "CAPSULE";
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.dense;

      // Dense model utilizes fold column zero values to calculate precise memory requirements
      denseModel = (XGBoostModel) Scope.track_generic(new hex.tree.xgboost.XGBoost(parms).trainModel().get());
      assertNotNull(denseModel);

      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.sparse;
      sparseModel = (XGBoostModel) Scope.track_generic(new hex.tree.xgboost.XGBoost(parms).trainModel().get());
      assertNotNull(sparseModel);


      Log.info(denseModel);
    } finally {
      if(denseModel != null) denseModel.deleteCrossValidationModels();
      if(sparseModel != null) sparseModel.deleteCrossValidationModels();
      Scope.exit();
    }
  }

  @Test
  public void testSparsityDetection(){
    Scope.enter();
    XGBoostModel sparseModel = null;
    XGBoostModel denseModel = null;
    try {

      // Fill ratio 0.2 (response not counted)
      Frame sparseFrame = Scope.track(new TestFrameBuilder()
              .withName("sparse_frame")
              .withColNames("C1", "C2", "C3")
              .withVecTypes(Vec.T_NUM,Vec.T_NUM,Vec.T_NUM)
              .withDataForCol(0, ard( 1,0, 0, 0, 0))
              .withDataForCol(1, ard( 0, 1, 0, 0, 0))
              .withDataForCol(2, ard( 2, 1,1, 4, 3))
              .build());


      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = sparseFrame._key;
      parms._response_column = "C3";
      parms._seed = 42;
      parms._backend = XGBoostModel.XGBoostParameters.Backend.cpu;
      parms._ntrees = 1;
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;

      sparseModel = (XGBoostModel) Scope.track_generic(new hex.tree.xgboost.XGBoost(parms).trainModel().get());
      assertNotNull(sparseModel);
      assertTrue(sparseModel._output._sparse);

      // Fill ratio 0.3 (response not counted) - the threshold for sprase is >= 0.3
      Frame denseFrame = Scope.track(new TestFrameBuilder()
              .withName("sparse_frame")
              .withColNames("C1", "C2", "C3")
              .withVecTypes(Vec.T_NUM,Vec.T_NUM,Vec.T_NUM)
              .withDataForCol(0, ard( 1,0, 0, 0, 0))
              .withDataForCol(1, ard( 1, 1, 0, 0, 0))
              .withDataForCol(2, ard( 2, 1,1, 4, 3))
              .build());

      parms._train = denseFrame._key;
      parms._response_column = "C3";
      parms._seed = 42;
      parms._backend = XGBoostModel.XGBoostParameters.Backend.cpu;
      parms._ntrees = 1;
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;

      // Dense model utilizes fold column zero values to calculate precise memory requirements
      denseModel = (XGBoostModel) Scope.track_generic(new hex.tree.xgboost.XGBoost(parms).trainModel().get());
      assertNotNull(denseModel);
      assertFalse(denseModel._output._sparse);

      Log.info(sparseModel);
    } finally {
      if(sparseModel != null) sparseModel.deleteCrossValidationModels();
      if(denseModel != null) denseModel.deleteCrossValidationModels();
      Scope.exit();
    }
  }

  @Test
  public void testMojoBoosterDump() throws IOException { 
    Assume.assumeFalse(XGBoostMojoReader.useJavaScoring(true, null));
    Scope.enter();
    try {
      Frame tfr = Scope.track(parse_test_file("./smalldata/prostate/prostate.csv"));

      Scope.track(tfr.replace(1, tfr.vecs()[1].toCategoricalVec()));   // Convert CAPSULE to categorical
      Scope.track(tfr.replace(3, tfr.vecs()[3].toCategoricalVec()));   // Convert RACE to categorical
      DKV.put(tfr);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = tfr._key;
      parms._response_column = "AGE";
      parms._ignored_columns = new String[]{"ID"};
      parms._seed = 42;
      parms._ntrees = 7;

      XGBoostModel model = (XGBoostModel) Scope.track_generic(new hex.tree.xgboost.XGBoost(parms).trainModel().get());
      Log.info(model);

      XGBoostMojoModel mojo = getMojo(model);
      assertTrue(mojo instanceof XGBoostNativeMojoModel);

      String[] dump = ((XGBoostNativeMojoModel) mojo).getBoosterDump(false, "text");
      assertEquals(parms._ntrees, dump.length);
    } finally {
      Scope.exit();
    }
  }

  /**
   * PUBDEV-5816: Tests correctness of training metrics returned for Multinomial XGBoost models
   */
  @Test
  public void testPubDev5816() {
    Scope.enter();
    try {
      final String response = "cylinders";

      Frame f = parse_test_file("./smalldata/junit/cars.csv");
      f.replace(f.find(response), f.vecs()[f.find(response)].toCategoricalVec()).remove();
      DKV.put(Scope.track(f));

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = response;
      parms._train = f._key;
      parms._ignored_columns = new String[]{"name"};

      XGBoostModel model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Scope.track_generic(model);

      ModelMetricsMultinomial modelMetricsMultinomial = (ModelMetricsMultinomial) model._output._training_metrics;

      Frame predicted = Scope.track(model.score(f));
      predicted.remove(0);
      ModelMetricsMultinomial expected = (ModelMetricsMultinomial.make(predicted, f.vec(response), f.vec(response).domain()));
      Scope.track_generic(expected);

      checkMetrics(expected, modelMetricsMultinomial);
    } finally {
      Scope.exit();
    }
  }

  @Test
  public void testMonotoneConstraints() {
    Scope.enter();
    try {
      final String response = "power (hp)";
      Frame f = parse_test_file("./smalldata/junit/cars.csv");
      f.replace(f.find(response), f.vecs()[f.find("cylinders")].toNumericVec()).remove();
      DKV.put(Scope.track(f));

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = response;
      parms._train = f._key;
      parms._ignored_columns = new String[] { "name" };
      parms._seed = 42;
      parms._reg_lambda = 0;
      parms._tree_method = XGBoostModel.XGBoostParameters.TreeMethod.hist;

      XGBoostModel.XGBoostParameters noConstrParams = (XGBoostModel.XGBoostParameters) parms.clone();
      XGBoostModel noConstrModel = new hex.tree.xgboost.XGBoost(noConstrParams).trainModel().get();
      Scope.track_generic(noConstrModel);

      assertTrue(ArrayUtils.contains(noConstrModel._output._varimp._names, "cylinders"));

      XGBoostModel.XGBoostParameters constrParams = (XGBoostModel.XGBoostParameters) parms.clone();
      constrParams._monotone_constraints = new KeyValue[] { new KeyValue("cylinders", -1) };
      XGBoostModel constrModel = new hex.tree.xgboost.XGBoost(constrParams).trainModel().get();
      Scope.track_generic(constrModel);

      // we essentially eliminated the effect of the feature by setting an inverted constraint
      assertFalse(ArrayUtils.contains(constrModel._output._varimp._names, "cylinders"));
    } finally {
      Scope.exit();
    }
  }

  @Test
  public void testValidateMonotoneConstraints() {
    Scope.enter();
    try {
      final String response = "power (hp)";

      Frame f = parse_test_file("smalldata/junit/cars.csv");
      Scope.track(f);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.auto;
      parms._response_column = response;
      parms._train = f._key;
      parms._seed = 42;

      thrown.expect(H2OModelBuilderIllegalArgumentException.class);
      thrown.expectMessage(CoreMatchers.containsString(
              "Details: ERRR on field: _monotone_constraints: Invalid constraint - column 'name' has type Enum. Only numeric columns can have monotonic constraints."));
      Model m = trainWithConstraints(parms, new KeyValue("name", -1));
      assertNotNull(m); // shouldn't be reached
      assertFalse(true);
    } finally {
      Scope.exit();
    }
  }

  @Test
  public void testMakeDataInfo() {
    Scope.enter();
    try {
      Frame f = parse_test_file("smalldata/junit/cars.csv");
      Scope.track(f);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._response_column = "year";
      parms._train = f._key;

      DataInfo dinfo = hex.tree.xgboost.XGBoost.makeDataInfo(f, null, parms, 1);
      assertNotNull(dinfo._coefNames);
    } finally {
      Scope.exit();
    }
  }

  @Test
  public void testScoreApproxContributions() throws IOException, XGBoostError {
    Scope.enter();
    try {
      Frame tfr = Scope.track(parse_test_file("./smalldata/junit/weather.csv"));
      assertEquals(1, tfr.anyVec().nChunks()); // tiny file => should always fit in a single chunk
      
      String response = "RainTomorrow";
      Scope.track(tfr.replace(tfr.find(response), tfr.vecs()[tfr.find(response)].toCategoricalVec()));
      // remove columns correlated with the response
      tfr.remove("RISK_MM").remove();
      tfr.remove("EvapMM").remove();
      DKV.put(tfr);

      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._ntrees = 5;
      parms._max_depth = 5;
      parms._train = tfr._key;
      parms._response_column = response;
      parms._save_matrix_directory = tmp.newFolder("matrix_dump").getAbsolutePath();

      XGBoostModel model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Scope.track_generic(model);
      Log.info(model);

      Frame contributions = model.scoreContributions(tfr, Key.<Frame>make(), true);
      Scope.track(contributions);

      assertEquals("BiasTerm", contributions.names()[contributions.names().length - 1]);
      
      // basic sanity check - contributions should sum-up to predictions
      Frame predsFromContributions = new CalcContribsTask().doAll(Vec.T_NUM, contributions).outputFrame();
      Frame expectedPreds = model.score(tfr);
      Scope.track(expectedPreds);
      assertVecEquals(expectedPreds.vec(2), predsFromContributions.vec(0), 1e-6);

      // make the predictions with XGBoost
      Map<String, String> rabitEnv = new HashMap<>();
      rabitEnv.put("DMLC_TASK_ID", "0");
      Rabit.init(rabitEnv);
      Booster booster = model.model_info().deserializeBooster();
      DMatrix matrix = new DMatrix(new File(parms._save_matrix_directory, "matrix.part0").getAbsolutePath());
      final float[][] expectedContribs = booster.predictContrib(matrix, 0);
      booster.dispose();
      matrix.dispose();
      Rabit.shutdown();
      
      // finally check the contributions
      assertEquals(expectedContribs.length, contributions.numRows());
      new CheckContribsTask(expectedContribs).doAll(contributions).outputFrame();
    } finally {
      Scope.exit();
    }
  }

  private static class CalcContribsTask extends MRTask<CalcContribsTask> {
    @Override
    public void map(Chunk[] cs, NewChunk nc) {
      for (int i = 0; i < cs[0]._len; i++) {
        float sum = 0;
        for (Chunk c : cs)
          sum += c.atd(i);
        nc.addNum(sigmoid(sum));
      }
    }
    private float sigmoid(float x) {
      return (1f / (1f + (float) Math.exp(-x)));
    }
  }
  
  private static class CheckContribsTask extends MRTask<CheckContribsTask> {
    private final float[][] _expectedContribs;

    private CheckContribsTask(float[][] expectedContribs) {
      _expectedContribs = expectedContribs;
    }

    @Override
    public void map(Chunk[] cs) {
      for (int i = 0; i < cs[0]._len; i++) {
        for (int j = 0; j < cs.length; j++) {
          float contrib = (float) cs[j].atd(i);
          int row = (int) cs[0].start() + i;
          assertEquals("Contribution in row=" + row + " on position=" + j + " should match.",
                  _expectedContribs[row][j], contrib, 1e-6);
        }
      }
    }
  }
  
  @Test
  public void testScoringWithUnseenCategoricals() {
    try {
      Scope.enter();
      final Frame training = new TestFrameBuilder()
              .withName("trainFrame")
              .withColNames("y", "x1", "x2")
              .withVecTypes(Vec.T_NUM, Vec.T_CAT, Vec.T_CAT)
              .withDataForCol(0, ard(0, 0, 2, 3, 4, 5.6, 7))
              .withDataForCol(1, ar("A", "B", "C", "E", "F", "I", "J"))
              .withDataForCol(2, ar("A", "B,", "A", "C", "A", "B", "A"))
              .withChunkLayout(2, 2, 2, 1)
              .build();
      Scope.track(training);

      final Frame test = new TestFrameBuilder()
              .withName("testFrame")
              .withColNames("y", "x1", "x2")
              .withVecTypes(Vec.T_NUM, Vec.T_CAT, Vec.T_CAT)
              .withDataForCol(0, ard(0, 0, 2, 3, 4, 5.6, 7))
              .withDataForCol(1, ar("X", "Y", "U", "W", "W", "Z", "Q"))
              .withDataForCol(2, ar("X", "Y,", "U", "W", "W", "Z", "Q"))
              .withChunkLayout(2, 2, 2, 1)
              .build();
      Scope.track(test);
      
      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._dmatrix_type = XGBoostModel.XGBoostParameters.DMatrixType.sparse;
      parms._response_column = "y";
      parms._train = training._key;
      parms._seed = 42;
      parms._ntrees = 5;

      XGBoostModel model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
      Scope.track_generic(model);

      Frame preds = model.score(test);
      Scope.track(preds);
      assertEquals(test.numRows(), preds.numRows());
    } finally {
      Scope.exit();
    }
  }

  @Test
  public void testScoreContributionsBernoulli() throws IOException, PredictException {
    Assume.assumeTrue("true".equalsIgnoreCase(confMojoJavaScoring));
    try {
      Scope.enter();
      Frame fr = Scope.track(parse_test_file("smalldata/junit/titanic_alt.csv"));
      fr.replace(fr.find("survived"), fr.vec("survived").toCategoricalVec());
      DKV.put(fr);
      XGBoostModel.XGBoostParameters parms = new XGBoostModel.XGBoostParameters();
      parms._train = fr._key;
      parms._distribution = bernoulli;
      parms._response_column = "survived";
      parms._ntrees = 5;
      parms._max_depth = 4;
      parms._min_rows = 1;
      parms._learn_rate = .2f;
      parms._score_each_iteration = true;
      parms._seed = 42;

      hex.tree.xgboost.XGBoost job = new hex.tree.xgboost.XGBoost(parms);
      XGBoostModel model = job.trainModel().get();
      Scope.track_generic(model);

      Frame contributions = model.scoreContributions(fr, Key.<Frame>make("contributions_titanic"));
      Scope.track(contributions);

      MojoModel mojo = model.toMojo();

      EasyPredictModelWrapper.Config cfg = new EasyPredictModelWrapper.Config()
              .setModel(mojo)
              .setEnableContributions(true);
      EasyPredictModelWrapper wrapper = new EasyPredictModelWrapper(cfg);

      for (long row = 0; row < fr.numRows(); row++) {
        RowData rd = toRowData(fr, model._output._names, row);
        BinomialModelPrediction pr = wrapper.predictBinomial(rd);
        assertArrayEquals("Contributions should match, row=" + row, 
                toNumericRow(contributions, row), ArrayUtils.toDouble(pr.contributions), 0);
      }
    } finally {
      Scope.exit();
    }
  }

  private static XGBoostModel trainWithConstraints(XGBoostModel.XGBoostParameters p, KeyValue... constraints) {
    XGBoostModel.XGBoostParameters parms = (XGBoostModel.XGBoostParameters) p.clone();
    parms._monotone_constraints = constraints;
    XGBoostModel model = new hex.tree.xgboost.XGBoost(parms).trainModel().get();
    Scope.track_generic(model);
    return model;
  }

  private static XGBoostMojoModel getMojo(XGBoostModel model) throws IOException {
    ByteArrayOutputStream os = new ByteArrayOutputStream();
    model.getMojo().writeTo(os);
    os.close();
    MojoReaderBackend mojoReaderBackend = MojoReaderBackendFactory.createReaderBackend(
            new ByteArrayInputStream(os.toByteArray()), MojoReaderBackendFactory.CachingStrategy.MEMORY);
    return (XGBoostMojoModel) MojoModel.load(mojoReaderBackend);
  }
  
  private static <T extends ModelMetricsSupervised> void checkMetrics(final T expectedMM, final T actualMM) {
    double precision = 1e-8;
    boolean doCheckCM = true;
    if (H2O.getCloudSize() >= 2) {
      precision = 5e-2; // results are non-deterministic
      doCheckCM = false; // CM can be about 5% different values
    }
    assertEquals(expectedMM.rmse(), actualMM.rmse(), precision);
    assertEquals(expectedMM._sigma, actualMM._sigma, precision);
    assertEquals(expectedMM._nobs, actualMM._nobs, precision);
    if (expectedMM instanceof ModelMetricsBinomial) {
      final ModelMetricsBinomial mmbExp = (ModelMetricsBinomial) expectedMM;
      final ModelMetricsBinomial mmbAct = (ModelMetricsBinomial) actualMM;
      assertEquals(mmbExp.logloss(), mmbExp.logloss(), precision);
      assertEquals(mmbExp.auc(), mmbAct.auc(), precision);
      assertEquals(mmbExp.mean_per_class_error(), mmbAct.mean_per_class_error(), precision);
      if (doCheckCM) {
        checkConfusionMatrix(mmbExp.cm(), mmbAct.cm());
      }
    } else if (expectedMM instanceof ModelMetricsMultinomial) {
      final ModelMetricsMultinomial mmmExp = (ModelMetricsMultinomial) expectedMM;
      final ModelMetricsMultinomial mmmAct = (ModelMetricsMultinomial) actualMM;
      assertEquals(mmmExp.logloss(), mmmAct.logloss(), precision);
      assertEquals(mmmExp.mean_per_class_error(), mmmAct.mean_per_class_error(), precision);
      if (doCheckCM) {
        checkConfusionMatrix(mmmExp.cm(), mmmAct.cm());
      }
    }
    assertArrayEquals(expectedMM.hr(), actualMM.hr(), (float) precision);
    assertArrayEquals(expectedMM._domain, actualMM._domain);
  }

  private static void checkConfusionMatrix(final ConfusionMatrix expectedCM, final ConfusionMatrix actualCM) {
    assertTrue("Expected: " + Arrays.deepToString(expectedCM._cm) + ", Got: " + Arrays.deepToString(actualCM._cm),
            Arrays.deepEquals(actualCM._cm, expectedCM._cm));
  }

}
