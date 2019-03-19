// function Person(firstname, lastname, age){
//     this.firstname = firstname;
//     this.lastname  = lastname;
//     this.age       = age;
// }

// var x = new Person('Hieu', 'Vo',1996);
// x.name = function(){
//     return this.firstname + this.lastname;
// }
// y = x.name();
// console.log(y);

var person = {
    firstname : 'Hieu',
    lastname  : 'Vo',
    age       : 21,
    fullname  : function(){
        return this.firstname +  this.lastname;
    }
}
x = person.fullname();
console.log(x)