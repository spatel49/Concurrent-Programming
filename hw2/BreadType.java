/**
 * Homework 2: Semaphores - Bakery/Customers
 * I pledge my honor that I have abided by the Stevens Honor System Date: 4 October 2020 Authors: Fabricio
 * Flores and Siddhanth Patel
 **/

public enum BreadType {
    RYE(3.99f), SOURDOUGH(4.99f), WONDER(5.99f);

    private float price;

    BreadType(float price) {
        this.price = price;
    }

    public float getPrice() {
        return price;
    }
}