package login;

import java.util.Map;
import java.util.HashMap;

public class Login {

  private Map<String,String> directory  = new HashMap<String,String>();

  private enum State {
    Unlogged, Logged;
  }

  private State state = State.Unlogged;
  
  public void login(String name, String password) throws LoginException {
    if(directory.get(name) == null)
      throw new LoginException("User " + name +" failed login");
    state = State.Logged;
  }
  

  public void logout() {
    state = State.Unlogged;
  }
  
}
