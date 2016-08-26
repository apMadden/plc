/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package myweatherclient;

import com.cdyne.ws.weatherws.ArrayOfWeatherDescription;
import com.cdyne.ws.weatherws.ForecastReturn;
import com.cdyne.ws.weatherws.WeatherReturn;

import java.util.Scanner;

/**
 *
 * @author Andy
 */
public class MyWeatherClient {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        Scanner scan = new Scanner(System.in);
        
        System.out.println("Weather Information");
        System.out.println();
        System.out.println();
        System.out.print("Enter a valid zipcode: ");        
        String zipCode = scan.next();
        System.out.println( "Zip code: " + zipCode);
        System.out.println( "City name: " + getCityWeatherByZIP(zipCode).getCity());
        System.out.println( "State name: " + getCityWeatherByZIP(zipCode).getState());
        System.out.println( "Temperature: " + getCityWeatherByZIP(zipCode).getTemperature() );
        System.out.println( "Relative Humidity: " + getCityWeatherByZIP(zipCode).getRelativeHumidity() );
        System.out.println( "Pressure: " + getCityWeatherByZIP(zipCode).getPressure() );
        System.out.println( "Wind: " + getCityWeatherByZIP(zipCode).getWind() );
    }

    private static ForecastReturn getCityForecastByZIP(java.lang.String zip) {
        com.cdyne.ws.weatherws.Weather service = new com.cdyne.ws.weatherws.Weather();
        com.cdyne.ws.weatherws.WeatherSoap port = service.getWeatherSoap();
        return port.getCityForecastByZIP(zip);
    }

    private static WeatherReturn getCityWeatherByZIP(java.lang.String zip) {
        com.cdyne.ws.weatherws.Weather service = new com.cdyne.ws.weatherws.Weather();
        com.cdyne.ws.weatherws.WeatherSoap port = service.getWeatherSoap();
        return port.getCityWeatherByZIP(zip);
    }

    private static ArrayOfWeatherDescription getWeatherInformation() {
        com.cdyne.ws.weatherws.Weather service = new com.cdyne.ws.weatherws.Weather();
        com.cdyne.ws.weatherws.WeatherSoap port = service.getWeatherSoap();
        return port.getWeatherInformation();
    }
    
}
