public class Fork {

    private final Object forkLock = new Object();
    private volatile boolean taken = false;

    public void take() {
        try {
            synchronized (forkLock) {
                while (taken) {
                    forkLock.wait();
                }

                assert !taken;
                taken = true;
            }
        } catch (InterruptedException e) {
            assert false;
        }
    }

    public void release() {
        synchronized (forkLock) {
            assert taken;
            taken = false;
            forkLock.notify();
        }
    }
}