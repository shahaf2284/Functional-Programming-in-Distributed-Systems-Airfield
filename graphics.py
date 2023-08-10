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
import random



LOG = logging.getLogger("GRAPHICS")
logging.getLogger("").setLevel(logging.DEBUG)


class MyProcess(Process):
    def __init__(self) -> None:
        Process.__init__(self)
        LOG.info(f"PASSIVE = {self.passive_}")
        self.get_node().register_name(self, Atom('graphics'))  # optional
        LOG.info("Registering process - 'graphics'")
        pygame.init()
        self.screen_width = 1200
        self.screen_height = 800
        self.clock = pygame.time.Clock()
        self.screen = pygame.display.set_mode((self.screen_width, self.screen_height))
        pygame.display.set_caption("Airfield image")
        self.background_image = pygame.image.load("/home/deki/Pyrlang/Airfield.png")  # Replace with your image file path
        self.background_image = pygame.transform.scale(self.background_image, (self.screen_width, self.screen_height))
        self.airplane1_image = pygame.image.load("/home/deki/Pyrlang/Ariplane1.png")
        self.airplane2_image = pygame.image.load("/home/deki/Pyrlang/Ariplane2.png")
        self.i = 0
        self.j = 0
        self.font = pygame.font.Font(None, 36)
        self.ETS = [dict(),dict(),dict(),dict()]
        # self.ETS[0].update({123:("airplane1",100,100,30)}) # PID=123, Type=Ariplane1,x=5,y=6
        LOG.info(f"mypid = {self.pid_}")


    def handle_one_inbox_message(self, msg):
        LOG.info("Incoming %s", msg)
        file = open(f"msg{self.i}.txt","w")
        file.write("msgNum"+str(self.i))
        file.write(f"msg={msg}")
        self.i +=1
        file.close()
        #self.ETS[0].update({123:("airplane1",random.randint(100,700),random.randint(100,700),random.randint(10,80))})
        
        
        #The message looks like (Atom('ets0'), [123,(Atom('airplane1'), 10, 10,30), (456,Atom('airplane2'), 80, 80,40)], [<1691659644.86.0 @ erl@127.0.0.1>])

        #msg[0] = which ETS(counting 0,1,2,3)
        #msg[1] = list of all objects
        #msg[2] = pid of the controller

        message_ets = int(str(msg[0])[3])
        self.ETS[message_ets]=dict()
        for item in msg[1]:
             self.ETS[message_ets].update({item[0]:(item[1],item[2],item[2],item[3])})
             

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
            self.clock.tick(30)
            #LOG.info(f"task done ={str(self.inbox_.task_done)}")
            try :
                msg = await self.receive()
                LOG.info("if msg if msg")
                LOG.info(msg)
                self.handle_one_inbox_message(msg)
            except:
                continue
                    
          
    def print_to_pygame(self):
        for ETS in self.ETS:
            for key in ETS.keys():
                val = ETS[key]
                model = val[0]
                (x,y) = (val[1],val[2])
                angle = val[3]
                if model == "airplane1":
                        scale_factor = 0.3
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
                if model == "airplane2":
                        scale_factor = 0.5
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

        self.j +=1
        # Add your game logic and other drawing operations here

    



def main():

    n = Node(node_name="py@127.0.0.1", cookie="COOKIE")
    process = MyProcess()
    n.run()

if __name__ == "__main__":
    main()
