# Scheme Module

LINFO2335 — Assignment Project 2 : 
	Functional Programming in **Scheme**

**Author** : *Ishimwe Jean-Paul*

**Noma** : *6919-12-00*

### [Step 0](./step_0.rkt): objects as dispatchers returning values
Emulating a point class as a constructor function that creates an object which is essentially a dispatch function that knows how to respond to messages:

- ``getx`` return x value of a point
- ``gety`` return y value of a point
- ``type`` return the type value of a point
- ``info`` display info about a point ``(point x y)``

### [Step 1](./step_1.rkt): objects as dispatchers returning functions
Making the dispatcher return, for each possible message, a function that knows how to treat that message:

* ``add`` add the current point and the parameter point and return a new point
* ``setx!`` change the value x of a point
* ``sety!`` change the value y of a point
* ``errArg`` display an error with the argument passed to the object

### [Step 2](./step_2.rkt): defining a message sending operator
Creating a *send operator*, so that we can write things like ``(send p 'getx)`` instead of this
ugly syntax ``((p 'getx))`` with two pairs of parentheses, and ``(send p1 'add p2)``
instead of ``((p1 'add) p2)``.

- ``send`` syntactic sugar to simplify sending messages to an object
- ``senderHelper`` help parse the arguments given to the ``send`` method
- ``typeCheck?`` checking if a variable is a correct object 
- ``errType`` display an error concerning the type of a variable (ie. not a valid object)


### [Step 3](./step_3b.rkt): introducing inheritance
Implementing a color−point class which inherits from point. In addition to the point’s coordinates it will keep a color attribute in its state. Redefining the appropriate methods of class color−point where needed, and delegating to the super class where it's possible.

- ``object`` base class
- ``method-lookup``
- ``object-wrapper`` when a *child* class calls on a *parent* class that return an object, we use this function to cast the *parent* object into a *child* object
- ``point``
	- ``super`` calling on object
	- ``substract`` 
	- ``div``
	- ``mul``
- ``color-point``
	- ``type``
	- ``setcolor!`` change the color of a color-point object
	- ``getcolor`` return the color
	- ``info`` display a color point information ``(color-point x y color)``
	- ``super`` calling on point


### [Step 4](./step_4.rkt): dynamic binding of self
Binding self to the instance being created. But it should do so recursively for all of its super classes as well, so that these know what self to call back in case of a self-send.

- ``point``
	- ``late-binding-test``
	- ``dummy-fun``
	- ``set-self!``
- ``color-point``
	- ``dummy-fun``
	- ``set-self!``
- ``new`` syntactic sugar to create a new object



### [Final step](./scheme_project.rkt): Typecast & Interface
Implementing a ``shape-interface`` which, when inherited from, should display an error if the class implementing it didn't define a method defined in the interface.

- ``point``
	- ``point-eq?`` checking if two points are equal
- ``shape-interface`` creating an interface. Trying to instantiate it should return an error
	- ``type``
	- ``add-point!``
	- ``del-point!``
	- ``get-points``
	- ``info``
- ``shape`` instantiating a shape interface
	- ``type`` return the type value of a shape
	- ``add-point!`` add a point to a shape
	- ``del-point!`` remove a point from a shape
	- ``info`` display shape information ``(shape (list of points))``