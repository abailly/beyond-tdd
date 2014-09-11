/**
 * 
 */
package oqube.dab.app;

import java.lang.reflect.Method;
import java.util.Locale;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

/**
 * The main application class for DAB system. This class loads data from a
 * Spring configuration file according to option given: if option -u is set,
 * then GUI application is started, if -w is set then Web application is
 * started, else CLI application is launched. Each option is represented by a
 * different configuration file that must be in the classpath.
 * 
 * @author nono
 * 
 */
public class Main {

  /**
   * @param args
   */
  public static void main(String[] args) {
    String configFile = "application-cli.xml";
    for (int i = 0; i < args.length; i++) {
      if ("-w".equals(args[i])) {
        System.out.println("Lauching Web client application not implemented.");
      } else if ("-u".equals(args[i])) {
        System.out.println("Lauching GUI client application not implemented.");
      } else if ("-e".equals(args[i])) { // english
        System.out.println("Language is set to English");
        Locale.setDefault(Locale.UK);
      } else if ("-f".equals(args[i])) { // francais
        System.out.println("La langue utilisee est le francais");
        Locale.setDefault(Locale.FRANCE);
      } else if ("-c".equals(args[i])) { // configuration
        configFile = args[++i];
        System.out.println("Using '" + configFile + "' as configuration");
      } else {
        System.err.println("Unknown option " + args[i]);
      }
    }
    // start context
    AbstractApplicationContext context = null;
    System.out.println("Lauching CLI application...");
    context = new ClassPathXmlApplicationContext(configFile);
    context.start();
    // get main and start it
    Runnable o = (Runnable) context.getBean("main");
    Thread th = new Thread(o);
    th.start();
    try {
      th.join();
    } catch (InterruptedException e) {
      // stop child
      th.interrupt();
    }
    context.stop();
  }
}
