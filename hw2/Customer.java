
/**
    * Homework 2: Semaphores - Bakery/Customers 
    * I pledge my honor that I have abided by the Stevens Honor System  
    * Date: 4 October 2020 
    * Authors: Fabricio Flores and Siddhanth Patel
**/

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.ArrayList;

public class Customer implements Runnable {
    private Bakery bakery;
    private Random rnd;
    private List<BreadType> shoppingCart;
    private int shopTime;
    private int checkoutTime;

    /**
     * Initialize a customer object and randomize its shopping cart
     */
    public Customer(Bakery bakery) {
        this.bakery = bakery;
        shoppingCart = new ArrayList<BreadType>();
        rnd = new Random();
        fillShoppingCart();
        this.shopTime = rnd.nextInt(1) + 1;
        this.checkoutTime = rnd.nextInt(1) + 1;
    }

    /**
     * Run tasks for the customer
     */
    public void run() {
        // TODO
        for (int i = 0; i < shoppingCart.size(); i++) {
            System.out.println("Customer " + hashCode() + " has began shopping.");
            try {
                Thread.sleep(shopTime);
            } catch (InterruptedException e1) {
                e1.printStackTrace();
            }
            if (shoppingCart.get(i) == BreadType.RYE) {
                try {
                    Bakery.permToGetRYE.acquire();
                    bakery.takeBread(shoppingCart.get(i));
                    Bakery.permToGetRYE.release();
                    System.out.println("Customer " + hashCode() + " has picked RYE");
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            } else if (shoppingCart.get(i) == BreadType.SOURDOUGH) {
                try {
                    Bakery.permToGetSourDough.acquire();
                    bakery.takeBread(shoppingCart.get(i));
                    Bakery.permToGetSourDough.release();
                    System.out.println("Customer " + hashCode() + " has picked SOURDOUGH");
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            } else {
                try {
                    Bakery.permToGetWonder.acquire();
                    bakery.takeBread(shoppingCart.get(i));
                    Bakery.permToGetWonder.release();
                    System.out.println("Customer " + hashCode() + " has picked WONDER");
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            }
        }
        try {
            Thread.sleep(checkoutTime);
            Bakery.permToCheckOut.acquire();
            Bakery.mutex.acquire();
            bakery.addSales(getItemsValue());
            Bakery.mutex.release();
            Bakery.permToCheckOut.release();
            System.out.println("Customer " + hashCode() + " has finished.");
        } catch (InterruptedException e1) {
            e1.printStackTrace();
        }
    }

    /**
     * Return a string representation of the customer
     */
    public String toString() {
        return "Customer " + hashCode() + ": shoppingCart=" + Arrays.toString(shoppingCart.toArray()) + ", shopTime="
                + shopTime + ", checkoutTime=" + checkoutTime;
    }

    /**
     * Add a bread item to the customer's shopping cart
     */
    private boolean addItem(BreadType bread) {
        // do not allow more than 3 items, chooseItems() does not call more than 3 times
        if (shoppingCart.size() >= 3) {
            return false;
        }
        shoppingCart.add(bread);
        return true;
    }

    /**
     * Fill the customer's shopping cart with 1 to 3 random breads
     */
    private void fillShoppingCart() {
        int itemCnt = 1 + rnd.nextInt(3);
        while (itemCnt > 0) {
            addItem(BreadType.values()[rnd.nextInt(BreadType.values().length)]);
            itemCnt--;
        }
    }

    /**
     * Calculate the total value of the items in the customer's shopping cart
     */
    private float getItemsValue() {
        float value = 0;
        for (BreadType bread : shoppingCart) {
            value += bread.getPrice();
        }
        return value;
    }
}