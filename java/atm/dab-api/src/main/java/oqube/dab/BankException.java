package oqube.dab;

public class BankException extends java.lang.Exception implements java.io.Serializable {
    public BankException(){};
    public BankException(java.lang.String reason) {
        super(reason);
    }
}
