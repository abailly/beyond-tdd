package oqube.dab;

public final class CardInvalidException extends BankException implements java.io.Serializable {
    public CardInvalidException(){};
    public CardInvalidException(java.lang.String reason) {
        super(reason);
    }
}
