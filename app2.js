class Person{
    constructor(height, weight){
        this.height = height;
        this.weight  = weight;
    }
    bmi(){
        return this.weight / Math.pow(this.height,2)

    }
}

var x = new Person(1.7,57);
console.log(Math.round(x.bmi()),2);