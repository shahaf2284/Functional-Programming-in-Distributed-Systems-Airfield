#
# This example shows:
# 1. Creating a Python object based on `Process` class, and registering it with
#   a name.
# 2. A remote node can send to this process by name or Pid.
# 3. On incoming message MyProcess'es `handle_inbox` will find that there is a
#   message and will call `handle_one_inbox_message` which is overridden here.
# 4. There is no way to know sender unless you add return address into the msg.
#
# Steps:
# 1. Run `make erlshell` or `erl -name erl@127.0.0.1 -setcookie COOKIE`
# 2. Run `make example2` in another terminal
# 3. In Erlang shell send a message `{my_process, 'py@127.0.0.1'} ! hello`
#

import logging

from term import Atom
from pyrlang.node import Node
from pyrlang.process import Process
import asyncio
import sys
import time
import pygame
import os



LOG = logging.getLogger("GRAPHICS")
logging.getLogger("").setLevel(logging.DEBUG)


class MyProcess(Process):
    def __init__(self) -> None:
        Process.__init__(self)
        LOG.info(f"PASSIVE = {self.passive_}")
        self.get_node().register_name(self, Atom('graphics'))  # optional
        LOG.info("Registering process - 'graphics'")
        pygame.init()
        self.screen_width = 1024
        self.screen_height = 1024
        self.clock = pygame.time.Clock()
        self.screen = pygame.display.set_mode((self.screen_width, self.screen_height))
        pygame.display.set_caption("Airfield image")
        self.scaling_factor = 0.3
        current_folder = os.path.dirname(os.path.abspath(__file__))

        # Construct the path to your image file
        image_filename = "Airfield.png"
        image_path = os.path.join(current_folder, image_filename)
        self.background_image = pygame.image.load(image_path)  
        self.background_image = pygame.transform.scale(self.background_image, (self.screen_width, self.screen_height))
        image_filename = "Ariplane1.png"
        image_path = os.path.join(current_folder, image_filename)
        self.airplane1_image = pygame.image.load(image_path)
        # self.airplane1_image = pygame.transform.scale(self.airplane1_image,(int(self.airplane1_image.get_width() * self.scaling_factor),
        #                                                                   int(self.airplane1_image.get_height() * self.scaling_factor)))
        
        image_filename = "Ariplane2.png"
        image_path = os.path.join(current_folder, image_filename)
        self.airplane2_image = pygame.image.load(image_path)
        # self.airplane2_image = pygame.transform.scale(self.airplane2_image,(int(self.airplane2_image.get_width() * self.scaling_factor),
        #                                                                   int(self.airplane2_image.get_height() * self.scaling_factor)))
        self.i = 0
        self.j = 0
        self.font = pygame.font.Font(None, 36)
        self.ETS = [dict(),dict(),dict(),dict()]
        LOG.info(f"mypid = {self.pid_}")


    def handle_one_inbox_message(self, msg):
        # file = open(f"msg{self.i}.txt","w")
        # file.write("msgNum"+str(self.i))
        # file.write(f"msg={msg}")
        # file.close()
        self.i +=1

        #self.ETS[0].update({123:("airplane1",random.randint(100,700),random.randint(100,700),random.randint(10,80))})
        
        
        #The message looks like (Atom('ets0'), [123,(Atom('airplane1'), 10, 10,20,30,5), (456,Atom('airplane2'), 80, 80,40)], [<1691659644.86.0 @ erl@127.0.0.1>])

        #msg[0] = which ETS(counting 0,1,2,3)
        #msg[1] = list of all objects
        #msg[2] = pid of the controller

        message_ets = int(str(msg[0])[3])-1
        self.ETS[message_ets]=dict()
        for item in msg[1]:
             self.ETS[message_ets].update({item[0]:(item[1],item[2],item[3],item[4],item[5],item[6])})
             #pid: type, x,y,z ,angle , speed
             

        # if (msg == "quit"):
        #     self.file.close()
        #     exit()

    async def process_loop(self):
        """ Polls inbox in an endless loop.
            .. note::
                This will not be executed if the process was constructed with
                ``passive=True`` (the default). Passive processes should read
                their inbox directly from ``self.inbox_``.
        """
        while not self.is_exiting_:
            # If any messages have been handled recently, do not sleep
            # Else if no messages, sleep for some short time
            self.screen.blit(self.background_image, (0, 0))
            current_time = time.strftime("%H:%M:%S")
            text_surface = self.font.render(current_time, True, (255, 255, 255))  # White color
            text_rect = text_surface.get_rect(center=(self.screen_width // 2, self.screen_height // 2))
            # Draw the text on the screen
            self.screen.blit(text_surface, text_rect)
            self.print_to_pygame()
            pygame.display.flip()
            pygame.display.update()
            pygame.event.pump()
            self.clock.tick(60) #MIGHT BE IMPORTANT
            #LOG.info(f"task done ={str(self.inbox_.task_done)}")
            try :
                msg = await self.receive()
                #LOG.info(msg)
                self.handle_one_inbox_message(msg)
            except:
                continue
                    
          
    def print_to_pygame(self):
        for ETS in self.ETS:
            for pid in ETS.keys():
                val = ETS[pid]
                model = val[0]
                x=val[1]
                y=val[2]
                z=val[3]
                angle = val[4] +180 # we add - because for some reason the angle spins it counter clockwise
                speed = val[5]
                font = pygame.font.Font(None, 36)
                if model == "airplane1":
                        scale_factor = 0.2
                        image_width,image_height = self.airplane1_image.get_width(),self.airplane1_image.get_height()
                        rotated_image = pygame.transform.rotate(self.airplane1_image, angle)
                        scaled_image = pygame.transform.scale(rotated_image, (int(image_width * scale_factor), int(image_height * scale_factor)))
                        # Get the dimensions of the scaled image
                        scaled_width = scaled_image.get_width()
                        scaled_height = scaled_image.get_height()
                        # Calculate the new coordinates to keep the center of the image at (x, y)
                        new_x = x - (scaled_width - image_width) / 2
                        new_y = y - (scaled_height - image_height) / 2
                        self.screen.blit(scaled_image, (new_x, new_y))
                                # Render number above character's head
                        text = font.render(str(z), True, (255, 255, 255))
                        text_width = text.get_width()
                        text_height = text.get_height()
                        text_x = new_x - text_width // 2
                        text_y = new_y - 15 - text_height  # Adjust the vertical position as needed
                        self.screen.blit(text, (text_x, text_y))
                if model == "airplane2":
                        scale_factor = 0.2
                        image_width,image_height = self.airplane2_image.get_width(),self.airplane2_image.get_height()
                        rotated_image = pygame.transform.rotate(self.airplane2_image, angle)
                        scaled_image = pygame.transform.scale(rotated_image, (int(image_width * scale_factor), int(image_height * scale_factor)))
                        # Get the dimensions of the scaled image
                        scaled_width = scaled_image.get_width()
                        scaled_height = scaled_image.get_height()
                        # Calculate the new coordinates to keep the center of the image at (x, y)
                        new_x = x - (scaled_width - image_width) / 2
                        new_y = y - (scaled_height - image_height) / 2
                        self.screen.blit(scaled_image, (new_x, new_y))
                                # Render number above character's head
                        text = font.render(str(z), True, (255, 255, 255))
                        text_width = text.get_width()
                        text_height = text.get_height()
                        text_x = new_x - text_width // 2
                        text_y = new_y - 15 - text_height  # Adjust the vertical position as needed
                        self.screen.blit(text, (text_x, text_y))
        self.j +=1
        # Add your game logic and other drawing operations here

    



def main():

    n = Node(node_name="py@127.0.0.1", cookie="COOKIE")
    process = MyProcess()
    n.run()

if __name__ == "__main__":
    main()
