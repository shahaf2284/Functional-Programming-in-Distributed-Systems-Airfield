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

The different sensors are:


### controller
The controller is a parallel process which can handle the following situations:

* Fallen computer.
* Adding a process to monitor.
* Fallen process (which can be a plane or a strip).
* plane landing in strip on the map's borders.
* plane moving from one tower area to another.

### Hierarchical Monitor
![image](https://github.com/shahaf2284/Functional-Programming-in-Distributed-Systems-Airfield/assets/122786017/855cd8c3-a232-4a80-a74c-2a71315163c9)

### how we create the board:
The graphics arae implemented using the PyGame module , while communicating with the controller using Pyrlang.

![image](https://github.com/shahaf2284/Functional-Programming-in-Distributed-Systems-Airfield/assets/122786017/e670f4ea-10b9-4968-9a8e-b4c5013a424b)



