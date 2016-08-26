/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package sortclient;

/**
 *
 * @author Andy
 */
public class SortClient {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        String key = getKey();
        String result = mergeSort("12 23 45 34 11 5 6", key);
        System.out.println("Key: " + key );
        System.out.println("Sorted list: " + result );
    }

    private static String getKey() {
        org.tempuri.Service service = new org.tempuri.Service();
        org.tempuri.IService port = service.getBasicHttpBindingIService();
        return port.getKey();
    }

    private static String mergeSort(java.lang.String input, java.lang.String userKey) {
        org.tempuri.Service service = new org.tempuri.Service();
        org.tempuri.IService port = service.getBasicHttpBindingIService();
        return port.mergeSort(input, userKey);
    }
    
}
