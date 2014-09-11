/*______________________________________________________________________________
*
* Copyright 2005 Arnaud Bailly - NORSYS/LIFL
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
*
* (1) Redistributions of source code must retain the above copyright
*     notice, this list of conditions and the following disclaimer.
*
* (2) Redistributions in binary form must reproduce the above copyright
*     notice, this list of conditions and the following disclaimer in
*     the documentation and/or other materials provided with the
*     distribution.
*
* (3) The name of the author may not be used to endorse or promote
*     products derived from this software without specific prior
*     written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
* IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
* ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
* INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
* STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
* OF THE POSSIBILITY OF SUCH DAMAGE.
*______________________________________________________________________________
*
* Created on 22 janv. 2005
* 
*/
package oqube.dab;

/**
 * An interface representing remote end of ATM transactions.
 * 
 * @author nono
 * @version $Id: /local/fidl/fidl/src/main/java/Banque/Bank.java 115 2005-12-06T21:35:50.769357Z nono  $
 */
public interface Bank {

  /**
   * Retrieve the balance of given account.
   * 
   * @param accountNo the account to retrieve balance for.
   * @return account's balance.
   * @throws BankException if account does not exist or bank cannot be reached.
   */
  int balance(String accountNo)throws BankException ;

  /**
   * Withdraw some amount of money from Bank.
   * 
   * @param accountNo the account to withdraw money from. 
   * @param amount amount of money to retrieve. Must be positive. 
   * @return true if money can be retrieve, false otherwise.
   * @throws BankException if account does not exist.
   */
  boolean withdraw(String accountNo, int amount) throws BankException ;
  
  /**
   * Deposit some amount of money to Bank
   * 
   * @param accountNo the account to withdraw money from.
   * @param amount amount of money to deposit. Must be positive.
   * @throws BankException  if account does not exist.
   */
  void deposit(String accountNo, int amount)  throws BankException ;
}

/* 
 * $Log: Bank.java,v $
 * Revision 1.1  2005/02/02 14:24:04  bailly
 * added Bank implementations
 *
*/
