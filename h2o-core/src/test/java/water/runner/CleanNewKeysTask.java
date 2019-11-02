package water.runner;

import org.junit.Ignore;
import water.*;

/**
 * Cleans leaked keys between tests.
 */
@Ignore
public class CleanNewKeysTask extends KeysMRTask<CleanNewKeysTask> {

    @Override
    protected void setupLocal() {
        DKVManager.retain(LocalTestRuntime.initKeys.toArray(new Key[0]));
    }

}
