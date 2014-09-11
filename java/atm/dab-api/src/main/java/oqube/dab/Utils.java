/**
 *  Copyright (C) 2009 - OQube / Arnaud Bailly
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 * Created on Fri Mar 27 2009
 */
package oqube.dab;

/**
 * 
 * @author abailly@oqube.com
 * @version $Id$
 */
public class Utils {

  /**
   * Copy an array of ints by creating a new instance and copying
   * every elements from 0.
   * This function exists in JDK6.
   * @param source the source array. Must not be null.
   * @param length length of array to copy. Must be lower or equal than
   * source.length.
   * @return a new array instance of the same lenght than source with
   * content copied from it.
   */
  static public int[] copyOf(int[] source, int length) {
    int[] dest = new int[length];
    System.arraycopy(source,0,dest,0,length); 
    return dest;
  }


}
