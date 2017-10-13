public class DiningPhilosophers {

    public static void main(String[] arg) {

        //          p2
        //       f1    f2
        //    p1          p3
        //       f4    f3
        //          p4

        Fork f1 = new Fork();
        Fork f2 = new Fork();
        Fork f3 = new Fork();
        Fork f4 = new Fork();

        Philosopher p1 = new Philosopher(f1, f4);
        Philosopher p2 = new Philosopher(f2, f1);
        Philosopher p3 = new Philosopher(f3, f2);
        Philosopher p4 = new Philosopher(f4, f3);

        Thread t1 = new Thread(p1);
        Thread t2 = new Thread(p2);
        Thread t3 = new Thread(p3);
        Thread t4 = new Thread(p4);

        t1.start();
        t2.start();
        t3.start();
        t4.start();

        try {
            t1.join();
            t2.join();
            t3.join();
            t4.join();
        } catch (InterruptedException e) {
            assert false;
        }
    }
}