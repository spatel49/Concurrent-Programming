/**
    * Homework 2: Semaphores - Bakery/Customers 
    * I pledge my honor that I have abided by the Stevens Honor System  
    * Date: 4 October 2020 
    * Authors: Fabricio Flores and Siddhanth Patel
**/


/* start the simulation */
public class Assignment2 {
    public static void main(String[] args) {
        Thread thread = new Thread(new Bakery());
        thread.start();

        try {
            thread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
