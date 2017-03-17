import math._
import scala.util._

/*
** Passer de joueurs en joueurs, tagguer dans la matirce toute les cases qu'il peut accéder ,et ainsi evaluer recursivement
 ** Mettre un compteur ou parcourir la matrice pour recuperer score maxi
 Attention ordre joueur */



/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  var DEPTH = 12
  var myMatrix = Array.ofDim[Int](20,30)
  var my_manipulated = Array.ofDim[Int](20,30)
  var distance_m = Array.ofDim[Int](20,30)
  var my_player = 0
  def is_available(posx : Int, posy : Int, matrix : Array[Array[Int]]) : Boolean =
  {
    if (posx >= 20 ||posy >= 30 ||posx < 0 ||posy < 0) {return false }
    return matrix(posx)(posy) == 0
  }



  def print_matrix(matrix:  Array[Array[Int]]) =
  {
    for (i <- 0 until 20)
    {
      for (j <- 0 until 30)
      {
        System.err.print(matrix(i)(j))
      }
      System.err.println()
    }
  }

  def calcul_score(): Long =
  {
      System.err.println("Calcul valeur pour p = "+my_player)
    var score = 0
    for (i <- 0 until 20)
    {
      for (j <- 0 until 30)
      {

        if (my_manipulated(i)(j) == (my_player + 1)) {score += 1000 }
        else if (my_manipulated(i)(j) != 0 && my_manipulated(i)(j) != (my_player + 1))
        {
          score -= 10
          score += - distance_m(i)(j) // Distance ennemy
        }

      }
    }
    score
  }

  def refresh()  ={
    for (i <- 0 until 20)
    {
      for (j <- 0 until 30)
      {
        my_manipulated(i)(j) = myMatrix(i)(j)

          distance_m(i)(j) = 0

      }
    }
  }
  def expand( depth : Int,  occupied : scala.collection.mutable.Queue[(Int, Int, Int)]): Long = {
    //  System.err.println("Depth "+depth + " Size queue "+occupied)
    var local_queue = occupied.clone()
    var changed = false
    //  System.err.println("Verification non perte "+occupied.size)
    val elem = local_queue.dequeue()
    val posx = elem._1
    val posy = elem._2
    val player = elem._3
    //  System.err.println("Verification non perte "+occupied.size+" "+local_queue.size)
    //System.err.println("Appel expand " + queue.size)
    if (is_available(posx + 1, posy, my_manipulated)) {
      changed = true
      local_queue += ((posx + 1, posy, player))
      my_manipulated(posx + 1)(posy) = player + 1
      distance_m(posx + 1)(posy) = depth
    }

    if (is_available(posx - 1, posy, my_manipulated)) {
      changed = true

      local_queue += ((posx - 1, posy, player))
      my_manipulated(posx - 1)(posy) = player + 1
      distance_m(posx - 1)(posy) = depth
    }

    if (is_available(posx, posy + 1, my_manipulated)) {
      changed = true
      local_queue += ((posx, posy + 1, player))
      my_manipulated(posx)(posy + 1) = player + 1
      distance_m(posx )(1 + posy) = depth
    }

    if (is_available(posx, posy - 1, my_manipulated)) {
      changed = true
      local_queue += ((posx, posy - 1, player))
      my_manipulated(posx)(posy - 1) = player + 1
      distance_m(posx )(posy - 1) = depth
    }
    if (local_queue.size != 0)
    {
      expand(depth + 1, local_queue)
    }
    else
    {
        System.err.println("Stop at depth "+depth)
      return depth +  calcul_score()
  }
  }


  /*
    def calcul_neighbours(posx : Int, posy : Int, depth : Int, player : Int, matrix : Array[Array[Int]])  =
    {
      if (is_available(posx + 1, posy, matrix))
      {
        matrix(posx+1)(posy) = player + 1
      }

      if (is_available(posx - 1, posy, matrix))
      {
        matrix(posx-1)(posy) = player + 1
      }

      if (is_available(posx , posy + 1, matrix))
      {
        matrix(posx)(posy + 1) = player + 1
        distance_m(posx)(posy + 1) = depth
      }

      if (is_available(posx , posy - 1, matrix))
      {
        matrix(posx)(posy - 1) = player + 1
        distance_m(posx)(posy - 1) = depth
      }

    }*/
  var occupied = new scala.collection.mutable.Queue[(Int, Int, Int)]
  var manipulated_q = new scala.collection.mutable.Queue[(Int, Int, Int)]

  // game loop
  while(true) {
    val begin = System.currentTimeMillis()
    // n: total number of players (2 to 4).
    // p: your player number (0 to 3).
    val Array(n, p) = for (i <- readLine split " ") yield i.toInt
    var my_x = 0
    var my_y = 0
    my_player = p
    var my_p = my_player
    manipulated_q.clear()
    for (i <- 0 until n) {
      // x0: starting X coordinate of lightcycle (or -1)
      // y0: starting Y coordinate of lightcycle (or -1)
      // x1: starting X coordinate of lightcycle (can be the same as X0 if you play before this player)
      // y1: starting Y coordinate of lightcycle (can be the same as Y0 if you play before this player)
      val Array(y0, x0, y1, x1) = for (i <- readLine split " ") yield i.toInt
 //   System.err.println("Value "+x1+" "+y1+" Player "+i)
      if (x1 >= 0 && y1 >= 0 && x0 >= 0 && y0 >= 0) {
        myMatrix(x1)(y1) = i + 1
        myMatrix(x0)(y0) = i + 1
        //     if (tour == 1)
        //       {
        //   occupied += ((x0,y0, i))
        //     }
        if (i != p)
          {
            occupied += ((x1, y1, i))
            manipulated_q += ((x1, y1, i))
          }

      }
      else {
        for (k <- 0 until 20) {
          for (j <- 0 until 30) {
            if (myMatrix(k)(j) == (i + 1)) {
              myMatrix(k)(j) = 0
              my_manipulated(k)(j) = 0
            }
          }
        }
      }
      if (i == p) {
        my_x = x1
        my_y = y1
        my_p = p
      }

    }

    // Cree une copie de la matrice qu'on va manipulé */
    for (i <- 0 until 20) {
      for (j <- 0 until 30) {

        my_manipulated(i)(j) = myMatrix(i)(j)

      }
    }

    var best_Score = Long.MinValue
    var move = ""


    refresh()
    // Enqueue move simulé car on le jouera apres
    if (is_available(my_x, my_y - 1, myMatrix)) {
        refresh()

      manipulated_q += ((my_x, my_y - 1, p))
      distance_m(my_x)(my_y - 1) = 1
      my_manipulated(my_x)(my_y - 1) = p + 1
      val score_L: Long = expand(1, manipulated_q)
      if (score_L > best_Score) {
        best_Score = score_L
        move = "LEFT"
      }
      System.err.println("Affichage matrice apres LEFT")
          print_matrix(my_manipulated)
      System.err.println("Score L "+score_L)
      val elem = manipulated_q.reverse
      distance_m(my_x)(my_y - 1) = 0
      elem.dequeue() // Suppression de l'élement simulé
      manipulated_q = elem.reverse
      my_manipulated(my_x)(my_y - 1) = 0
    }


    if (is_available(my_x, my_y + 1, myMatrix)) {

      refresh()
     my_manipulated(my_x)(my_y + 1) = p + 1
      distance_m(my_x)(my_y + 1) = 1
      manipulated_q += ((my_x, my_y + 1, p))

      val score_R: Long = expand(1, manipulated_q)
      if (score_R > best_Score) {
        best_Score = score_R
        move = "RIGHT"
      }
      //System.err.println("Score R "+score_R)
      val elem = manipulated_q.reverse
      elem.dequeue()
      manipulated_q = elem.reverse
      //System.err.println("Affichage right")
      //print_matrix(my_manipulated)
      my_manipulated(my_x)(my_y + 1) = 0

      distance_m(my_x)(my_y + 1) = 0
    }

    if (is_available(my_x - 1, my_y, myMatrix)) {
        refresh()
      manipulated_q += ((my_x - 1, my_y, p))
      my_manipulated(my_x - 1)(my_y) = p + 1
      distance_m(my_x - 1)(my_y ) = 1
      refresh()
      val score_U: Long = expand(1, manipulated_q)
      if (score_U > best_Score) {
        best_Score = score_U
        move = "UP"
      }
      distance_m(my_x - 1)(my_y ) = 0
      System.err.println("Score U "+score_U)

      val elem = manipulated_q.reverse
      elem.dequeue()
      manipulated_q = elem.reverse
        //System.err.println("Affichage UP")
      //print_matrix(my_manipulated)
      my_manipulated(my_x - 1)(my_y) = 0
  }
    //  System.err.println("Verification size manipulated q "+manipulated_q.size)
    if (is_available(my_x + 1, my_y, myMatrix)) {
        refresh()
      //System.err.println("Down avant ")
      //print_matrix(my_manipulated)
      my_manipulated(my_x + 1)(my_y) = p + 1
      distance_m(my_x + 1)(my_y ) = 1
      manipulated_q += ((my_x + 1, my_y, p))
      val score_D: Long = expand(1, manipulated_q)
      refresh()
      if (score_D > best_Score) {
        best_Score = score_D
        move = "DOWN"
      }
      distance_m(my_x + 1)(my_y ) = 0

      //System.err.println("Score D "+score_D)
      val elem = manipulated_q.reverse
      elem.dequeue()
      my_manipulated(my_x + 1)(my_y) = 0
      //System.err.println("Affichage Down ")
    //print_matrix(my_manipulated)
    }
    val end = System.currentTimeMillis()

    /*  System.err.println("Temps "+(end - begin))
      print_matrix(myMatrix)
      System.err.println("-----------")
      print_matrix(my_manipulated)*/

    println(move)

  }
}
