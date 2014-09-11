package oqube.dab;

public class Card {

  private final String pin;

  private final String account;

  private int failedCode;

  public String getPin() {
    return pin;
  }

  public String getAccount() {
    return account;
  }

  public int getFailedCode() {
    return failedCode;
  }

  public Card(String pin, String account, int failedCode) {
    this.pin = pin;
    this.account = account;
    this.failedCode = failedCode;
  }

  public boolean checkCode(String code) {
    boolean ret = code.equals(pin);
    if (!ret)
      failedCode++;
    return ret;
  }

}
