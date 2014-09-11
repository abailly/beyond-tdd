/*______________________________________________________________________________
 * 
 * Copyright 2006  Arnaud Bailly - 
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
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Created on Thu Jun 29 2006
 *
 */
package oqube.dab;

/**
 * An interface for communicating with the notes-storage and distribution system.
 * 
 * @author abailly@norsys.fr
 * @version $Id$
 */
public interface Dispenser {

  /**
   * the values of available bank notes in dispenser.
   */
  int[]notes = new int[]{10,20,50,100};

  /**
   * Ask dispenser to deliver right amount
   * of bank notes with given distribution.
   * @exception BankException if delivery breakdown is not available
   */
  void deliver(int[] notes) throws BankException ; 

  /**
   * Return the number of banknotes available for each kind.
   * By convention, the integer array returned has 4 slots 
   * for 10,20,50 and 100 euros notes.
   */
  int[] getReserve();

}
