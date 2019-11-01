package hex;

import jsr166y.ForkJoinTask;
import water.util.IcedAtomicInt;
import water.util.Log;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Dispatcher for parallel model building. Starts building models every time the run method is invoked.
 * After each model is finished building, the `modelFinished` method is invoked, which in turn invokes modelFeeder callback.
 * ModelFeeder receives the model built and can deal with it in any way - e.g. put it into a Grid, or discard it if the resulting model
 * is a failure. It also has the power to invoke the training of any number of new models. Or stop the parallel model builder,
 * released the barrier inside.
 */
public class ParallelModelBuilder extends ForkJoinTask<ParallelModelBuilder> {

  private final BiConsumer<Model, ParallelModelBuilder> _onBuildSuccessCallback;
  private final BiConsumer<ModelBuildFailure, ParallelModelBuilder> _onBuildFailureCallback;
  public final IcedAtomicInt _modelInProgressCounter = new IcedAtomicInt();
  public final AtomicBoolean _completed = new AtomicBoolean(false);

  public ParallelModelBuilder(BiConsumer<Model, ParallelModelBuilder> onBuildSuccessCallback,
                              BiConsumer<ModelBuildFailure, ParallelModelBuilder> onBuildFailureCallback) {
    Objects.requireNonNull(onBuildSuccessCallback);
    _onBuildSuccessCallback = onBuildSuccessCallback;
    _onBuildFailureCallback = onBuildFailureCallback;
  }

  /**
   * Runs given collection of {@link ModelBuilder} in parallel. After each model is finished building,
   * one of the callbacks (on model failure / on model completion) is called.
   *
   * @param modelBuilders An {@link Collection} of {@link ModelBuilder} to execute in parallel.
   */
  public void run(final Collection<ModelBuilder> modelBuilders) {
      for (final ModelBuilder modelBuilder : modelBuilders) {
        _modelInProgressCounter.incrementAndGet();

        // Set the callbacks
        final Consumer<Model> onModelSuccessfulBuild = this::onModelSuccessfullBuild;
        modelBuilder.setOnSuccessCallback(onModelSuccessfulBuild);
        final BiConsumer<Throwable, Model.Parameters> onModelFailedWithException = this::onModelFailedWithException;
        modelBuilder.setOnFailCallback(onModelFailedWithException);

        modelBuilder.trainModel();
      }
  }

  /**
   * Callback for successfully finished model builds
   *
   * @param m Model built
   */
  private void onModelSuccessfullBuild(final Model m) {
    _modelInProgressCounter.decrementAndGet();
    _onBuildSuccessCallback.accept(m, this);
    attemptComplete();
  }

  /**
   * Callback for failed model builds
   *
   * @param throwable  Cause of failure
   * @param parameters Parameters used in the attempt to build the model
   */
  private void onModelFailedWithException(final Throwable throwable, final Model.Parameters parameters) {
    _modelInProgressCounter.decrementAndGet();

    final ModelBuildFailure modelBuildFailure = new ModelBuildFailure(throwable, parameters);
    _onBuildFailureCallback.accept(modelBuildFailure, this);
    attemptComplete();
  }

  /**
   * Contains all the necessary information after a model builder has failed to build the model
   */
  public static class ModelBuildFailure {
    private final Throwable _throwable;
    private final Model.Parameters _parameters;

    public ModelBuildFailure(Throwable throwable, Model.Parameters parameters) {
      this._throwable = throwable;
      this._parameters = parameters;
    }

    public Throwable getThrowable() {
      return _throwable;
    }

    public Model.Parameters getParameters() {
      return _parameters;
    }
  }

  /**
   * Indicate this builder there will be no more models.
   */
  public void noMoreModels() {
    _completed.set(true);
  }
  
  private void attemptComplete(){
    if(!_completed.get() || _modelInProgressCounter.get() != 0) return;
    complete(this);
  }


  @Override
  public ParallelModelBuilder getRawResult() {
    return this;
  }

  @Override
  protected void setRawResult(ParallelModelBuilder value) {
  }

  @Override
  protected boolean exec() {
    return false;
  }
}
