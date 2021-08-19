访问者模式是一种将算法与对象结构分离的软件设计模式

这个模式的基本想法如下：首先我们拥有**一个由许多对象构成的对象结构**，这些对象的类都拥有一个`accept`方法用来接受访问者对象；**访问者是一个接口**，它拥有一个`visit`方法，这个方法对访问到的对象结构中不同类型的元素作出不同的反应；在对象结构的一次访问过程中，我们遍历整个对象结构，对每一个元素都实施`accept`方法，在每一个元素的`accept`方法中回调访问者的`visit`方法，从而**使访问者得以处理对象结构的每一个元素**。我们可以针对对象结构设计不同的实在的访问者类来完成不同的操作。

```java
interface Visitor {
     void visit(Wheel wheel);
     void visit(Engine engine);
     void visit(Body body);
     void visit(Car car);
 }

 class Wheel {
     private String name;
     Wheel(String name) {
         this.name = name;
     }
     String getName() {
         return this.name;
     }
     void accept(Visitor visitor) {
         visitor.visit(this);
     }
 }
  
 class Engine {
     void accept(Visitor visitor) {
         visitor.visit(this);
     }
 }

 class Body {
     void accept(Visitor visitor) {
         visitor.visit(this);
     }
 }

 class Car {
     private Engine  engine = new Engine();
     private Body    body   = new Body();
     private Wheel[] wheels 
         = { new Wheel("front left"), new Wheel("front right"),
             new Wheel("back left") , new Wheel("back right")  };
     void accept(Visitor visitor) {
         visitor.visit(this);
         engine.accept(visitor);
         body.accept(visitor);
         for (int i = 0; i < wheels.length; ++ i)
             wheels[i].accept(visitor);
     }
 }

 class PrintVisitor implements Visitor {
     public void visit(Wheel wheel) {
         System.out.println("Visiting " + wheel.getName()
                             + " wheel");
     }
     public void visit(Engine engine) {
         System.out.println("Visiting engine");
     }
     public void visit(Body body) {
         System.out.println("Visiting body");
     }
     public void visit(Car car) {
         System.out.println("Visiting car");
     }
 }

 public class VisitorDemo {
     static public void main(String[] args) {
         Car car = new Car();
         Visitor visitor = new PrintVisitor();
         car.accept(visitor);
     }
 }
```