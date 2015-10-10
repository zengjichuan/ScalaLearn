/**
 * Created by JC on 2015/10/9.
 */
package org
package object learningconcurrency {
  def log(msg: String): Unit =
    println(s"${Thread.currentThread().getName}: $msg")
}
