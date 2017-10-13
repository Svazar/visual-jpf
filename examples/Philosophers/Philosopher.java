public class Philosopher implements Runnable {

    private final Fork leftFork;
    private final Fork rightFork;

    public Philosopher(Fork leftFork, Fork rightFork) {
        this.leftFork = leftFork;
        this.rightFork = rightFork;
    }

    public void run() {
        eat();
    }

    public void eat() {
        leftFork.take();
        rightFork.take();

        // eating

        rightFork.release();
        leftFork.release();
    }
}