# Functional-Programming-in-Distributed-Systems-Airfield

In this project we implemented a airplane network based on a given communication Control Tower. The map contains flight paths (takeoff and landing) and hangar where the plane was created and destroyed. The plane has a random direction of movement in space and also speed
The system is comprised of several main components:
Airplanes with accompanied sensors, external servers(communication towers), srtip and the hangars that are in every strip.
All of the planes are state machines which move independently in parallel to one another.
Using communication towers along the map, the planes transfer information to external servers which make decisions regarding the continuation of their journey. Each car is accompanied with several processes which mimic sensors that monitor information from the plane's surroundings. This information creates an event in the plane's state machine, and the plane responds according to the event.
The strip map is divided into four regions, so that a different server is responsible for each region and for routing the planes in said region. These servers are located on different computers. Furhtermore, there is a fifth computer which is in charge of the graphic display of the system.


## The Systems' Components
### Plane
A Plane is a state machine implemented with gen_statem. The plane flies around the map and responds to it's surrounding using the information which is passed onto them by control tower. Some of the events require immediate attention and are attended to locally by the vehicle itself. The rest of the events are taken care of by the external tower (different process).
## State machine-plane:

![image](https://github.com/shahaf2284/Functional-Programming-in-Distributed-Systems-Airfield/assets/122786017/d7d09dde-2f91-45b7-a058-5a06ba7dfb69)


### controller
The controller is a parallel process which can handle the following situations:

* Fallen computer.
* Adding a process to monitor.
* Fallen process (which can be a plane or a strip).
* plane landing in strip on the map's borders.
* plane moving from one tower area to another.

### Hierarchical Monitor
![image](https://github.com/shahaf2284/Functional-Programming-in-Distributed-Systems-Airfield/assets/122786017/855cd8c3-a232-4a80-a74c-2a71315163c9)

## how we create the board:
The graphics arae implemented using the PyGame module , while communicating with the controller using Pyrlang.
### Graphics
*	The graphics are implemented with a python process, using the Pyrlang module to link between the erlang and python, and PyGame module to display our planes with images we collected on the internet.
*	The graphics receive an update periodically of the 4 ETS’s that the main controller possesses at each time slot, and simply display it.
# -------------------------------------------------------------

## Tools used - 
ETS (Erlang Term Storage) is a built-in feature in Erlang for creating and managing in-memory tables that store Erlang terms. ETS tables provide a way to efficiently store and manipulate data in a concurrent environment.
In our experiment, we needed to use ETS tables for fast lookup of planes, where our key was the airplane’s Process id, we could extract information such as its location and speed, to assert what we need to do with it(spin for example).
We used ETS tables in both the main controller and each control tower, constantly trying to sync between them.

### Gen server – 
OTP has a generic server module, which implements the abstraction for many methods of message handling and initializing the objects.  
In our case, the main controller and the control towers used the gen server behaviour,
The functions were described above.

### Gen state machine – 
The airplanes were state machines with 6 states.
We used the state functions callback module to transition between each state and didn’t rely too much on events, because our transitions were simple.
The functions were described above in the plane module.

### Pyrlang – 
Because we decided to implement the graphics with PyGame, we needed a tool to translate erlang to python, and so we used Pyrlang.
Pyrlang uses the Asyncio:Queue module to implement a message box, and thus allowing message passing like we needed.
We overrode a few functions to implement a while loop, that is necessary for PyGame to operate.

![image](https://github.com/shahaf2284/Functional-Programming-in-Distributed-Systems-Airfield/assets/122786017/220239c0-6d6e-4cd0-867b-dc707ac17154)
#### links- 
1- https://github.com/AntonioGallego/pyErlang
2- https://github.com/Pyrlang/Pyrlang
# -------------------------------------------------------------
PyGame – 
PyGame is a python library, often used to create simple games, but it can support simple GUI’s such as the one we needed here, we used images of a map, and a few airplane models, and using the coordinates we got from the ETS’s we could print the planes in their locations, and a small Z axis logo below the planes.

![image](https://github.com/shahaf2284/Functional-Programming-in-Distributed-Systems-Airfield/assets/122786017/e670f4ea-10b9-4968-9a8e-b4c5013a424b)



