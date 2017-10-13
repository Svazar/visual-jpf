rm -rf bin DiningPhilosophers.analysis DiningPhilosophers.error.log
mkdir bin
javac -d bin DiningPhilosophers.java

jpf.bat DiningPhilosophers.jpf 1> DiningPhilosophers.analysis 2> DiningPhilosophers.error.log

