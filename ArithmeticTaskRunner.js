class ArithmeticTaskRunner {
  constructor(){
    this.tasks = [];
  }

  addNegationTask(){
    this.tasks.push(
      function (x){
      return -x;
    })
  }

  addAdditionTask(y){
    this.tasks.push(
      function (x){
      return x+y;
    })
  }

  addMultiplicationTask(y){
    this.tasks.push(
      function (x){
      return x*y;
    })
  }

  get taskCount(){
    return this.tasks.length;
  }

  execute(value){
    if (value == null){
      value = 0;
    }
    this.tasks.forEach((task) => value = task(value));
    return value;
  }
}

let taskRunner = new ArithmeticTaskRunner()
taskRunner.addNegationTask();
taskRunner.addAdditionTask(20);
taskRunner.addMultiplicationTask(2);
console.log(taskRunner.execute(10));
console.log(taskRunner.execute());
