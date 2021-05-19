/**
 * cse250.pa5.HashTableMap.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: djjeong
 * Person#: 50270181
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa5

import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V]()(implicit hash: Hashing[K]) extends cse250.objects.Map[K, V] {
  var n = 0
  var N = 10
  var alpha: Double = 0.0
  val alphaMax: Double = 0.6

  var bucketArray = Array.fill[ListBuffer[(K,V)]](N)(ListBuffer[(K,V)]())

  def rehash(newSize: Int): Unit = {
    if (newSize > N) {
      val oldBucketArray = bucketArray
      n = 0
      N = newSize
      bucketArray = Array.fill[ListBuffer[(K,V)]](N)(ListBuffer[(K,V)]())
      alpha = n / N
      for (bucket <- oldBucketArray; elem <- bucket) addOne(elem)
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    var bucketIndex: Int = hash.hash(elem._1) % N
    var exists: Boolean = false

    if (bucketArray(bucketIndex).isEmpty) {
      n += 1
      alpha = n / N.toDouble
      if (alpha > alphaMax){
        rehash(N * 2)
        bucketIndex = hash.hash(elem._1) % N
        n += 1
      }
      bucketArray(bucketIndex).prepend(elem)
    }
    else {
      for (i <- bucketArray(bucketIndex)) {
        if (i._1 == elem._1) {
          exists = true
          bucketArray(bucketIndex)(bucketArray(bucketIndex).indexOf(i)) = elem
        }
      }
      if (!exists) {
        n += 1
        alpha = n / N.toDouble
        if (alpha > alphaMax){
          rehash(N * 2)
          n += 1
          bucketIndex = hash.hash(elem._1) % N
        }
        bucketArray(bucketIndex).prepend(elem)
      }
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % N
    for (elem <- bucketArray(lookupIndex)) {
      if (elem._1 == key) return Some(elem._2)
    }
    None
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    var found: Boolean = false
    var firstfound: Boolean = false
    var bucketindex: Int = 0
    var listindex: Int = 0


    override def hasNext: Boolean = {
      var has: Boolean = false
      for (index <- bucketindex until N) {
        if (bucketArray(index).nonEmpty) {
          has = true
        }
      }
      if (bucketArray(bucketindex).nonEmpty) {
        if (listindex < bucketArray(bucketindex).length - 1) {
          has = true
        }
      }
      has
    }

    override def next(): (K, V) = {
      var elem: (K, V) = null
      if (bucketindex < N) {
        if (bucketArray(bucketindex).nonEmpty) {
          if (listindex == bucketArray(bucketindex).length - 1) {
            elem = bucketArray(bucketindex)(listindex)
            listindex = 0
            bucketindex += 1
            for (index <- bucketindex until N) {
              if (bucketArray(index).nonEmpty && !found) {
                bucketindex = index
                found = true
              }
            }
          }
          else {
            elem = bucketArray(bucketindex)(listindex)
            listindex += 1
          }
        }
        //bucketArray(bucketindex) is empty
        else {
          for (index <- bucketindex until N) {
            if (bucketArray(index).nonEmpty && !firstfound) {
              bucketindex = index
              firstfound = true
            }
          }
          elem = bucketArray(bucketindex)(listindex)
          if (listindex == bucketArray(bucketindex).length - 1) {
            listindex = 0
            bucketindex += 1
            for (index <- bucketindex until N) {
              if (bucketArray(index).nonEmpty && !found) {
                bucketindex = index
                found = true
              }
            }
          }
          else {
            listindex += 1
          }
        }
      }
      found = false
      elem
    }
  }
}
