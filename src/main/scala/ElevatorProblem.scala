/**
  * Find if Elevator will fail.
  */
object ElevatorProblem {

  def main(args: Array[String]): Unit = {
    (1 to io.StdIn.readInt).foreach(_ => { // test cases
      val floors = io.StdIn.readInt
      val Array(maxPersonsAllowed, maxWeightAllowed) = io.StdIn.readLine.split(" ").map(_.toInt)
      val personsAtFloor: Array[Int] = io.StdIn.readLine.split(" ").map(_.toInt) // floors - 1

      var personsSoFar = 0
      var weightSoFar = 0

      val getDownWeight = new Array[Int](floors + 1)
      val getDownPersons = new Array[Int](floors + 1)

      var i = 1

      while (i <= floors && personsSoFar <= maxPersonsAllowed && weightSoFar <= maxWeightAllowed) {
        if (i != floors) {
          val desiredFloors = io.StdIn.readLine.split(" ").map(_.toInt)
          val weights = io.StdIn.readLine.split(" ").map(_.toInt)

          // update ahead
          desiredFloors.zip(weights).foreach { case (floor, weight) =>
            getDownWeight(floor) += weight
            getDownPersons(floor) += 1
          }

          // people getting in
          personsSoFar += personsAtFloor(i - 1)
          weightSoFar += weights.sum
        }

        // people getting out
        personsSoFar -= getDownPersons(i)
        weightSoFar -= getDownWeight(i)

        i += 1
      }
      println(i-1)
    })
  }
}

// Problem -

/*

Elevator Overload <Ninjacart>
A corporate building has n floors numbered from 1 to n. An elevator starts moving from the floor 1 and goes upward until it reaches floor the n. It has the maximum capacity of weight (W) and persons (P) it can lift. It will stop at every floor and there will be some number of employees who will get into the elevator and also some of the employees will leave it as they have reached their desired floors. At every floor, those who have reached the desired floor will leave first and then those who were waiting for the elevator will get in. As the elevator has a maximum limit of weight and persons (when any one of the limits is exceeded, the overload sign is displayed), it will permanently stop where the overload situation happens or it will permanently stop at floor n if no overload situation happens. You need to find the floor, where the elevator will stop permanently.

Note: If an employee gets in at floor x, then it is guaranteed that his desired floor will be greater than x and less than or equal to n. There are no employees waiting at floor n.

You have to answer for t number of test cases.

Input Format

The first line will contain one integer t, which denotes the number of test cases.
For every test case

The first line will have a single integer denoting the number of floors in the building i.e. n.
Second line will have two space separated integers representing the values of P and W
Third line will have a 1-indexed space separated array of  integers where  integer will represent the number of employees waiting for the elevator at floor i.
In next  lines, for every floor, there will be 2 lines of space separated integers where the first line represent the desired floor and second line will represent the corresponding weight of the employees waiting at that floor.
Output Format

Output will have single integer denoting the floor number where the elevator will permanently stop
Constraints

 number of employees at each floor
 weight of each employee
Sample Input
2
4
3 150
2 2 2
2 3
50 50
3 4
50 50
4 4
20 10
2
1 50
2
2 2
50 50
Sample Output
4
1
Explanation
In the first test case,

At floor 1, two employees will get in and the total weight will be 100
At floor 2, one employee of weight 50 will leave and two employees each of weight 50 will get in. Total number of employees will become 3 and total weight will be 150
At floor 3, two employees each of weight 50 will leave and two other employees of weight 20 and 10 will get in. Total number of employees will become 3 and total weight will become 80
The elevator will be able to reach floor number 4 without encountering an overload situation.
The answer is 4.

In the second case,

At floor 1, two employees of weight 50 and 50 will get in which will create an overload situation as both the number of employees and total weight is exceeding the maximum limit. So, the elevator will stop at floor number 1
The answer is 1

Note: Your code should be able to convert the sample input into the sample output. However, this is not enough to pass the challenge, because the code will be run on multiple test cases. Therefore, your code must solve this problem statement.

*/

