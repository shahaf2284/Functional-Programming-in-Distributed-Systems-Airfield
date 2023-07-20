import pygame

# Constants for the window size and pixel size
WINDOW_WIDTH = 1700
WINDOW_HEIGHT = 1000
PIXEL_SIZE = 10

GRID_WIDTH = WINDOW_WIDTH // PIXEL_SIZE
GRID_HEIGHT = WINDOW_HEIGHT // PIXEL_SIZE

# Colors
Singlepixel = (255, 255, 255)
Frame = (0, 0, 0)

def draw_pixel_board(screen):
    for x in range(0, WINDOW_WIDTH, PIXEL_SIZE):
        for y in range(0, WINDOW_HEIGHT, PIXEL_SIZE):
            pygame.draw.rect(screen, Frame, (x, y, PIXEL_SIZE, PIXEL_SIZE), 1)

def main():
    pygame.init()

    screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
    pygame.display.set_caption("Pixel Board")

    # Load the pixel image
    #Grass
    Grass = pygame.image.load("GRASS.png").convert()
    #Towers
    image_tower1 = pygame.image.load("tower1.png").convert()
    image_tower2 = pygame.image.load("tower2.png").convert()
    #corna
    corna_left = pygame.image.load("corna.png").convert()
    #Mount
    Mountain = pygame.image.load("mont.png").convert()
    #strip
    Strip = pygame.image.load("Strip.png").convert()
    Strip2 = pygame.image.load("Strip2.png").convert()
    #Aircraft parking
    garag = pygame.image.load("Aircraft parking.png").convert()

    # Reduce the image dimensions by scaling
    Grass = pygame.transform.scale(Grass, (1101, 598))
    image_tower1 = pygame.transform.scale(image_tower1, (50, 50))
    image_tower2 = pygame.transform.scale(image_tower2, (50, 50))
    corna_left = pygame.transform.scale(corna_left, (150, 150))
    Mountain = pygame.transform.scale(Mountain, (50, 80))
    Strip = pygame.transform.scale(Strip, (200, 13))
    Strip2 = pygame.transform.scale(Strip2, (250, 250))
    Strip3 = pygame.transform.scale(Strip2, (250, 250))
    garag = pygame.transform.scale(garag, (20, 20))

    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        #grass = pygame.image.load("GRASS.png").convert()
        #grass = pygame.transform.scale(grass, (PIXEL_SIZE, PIXEL_SIZE))
        screen.fill(Singlepixel)
        draw_pixel_board(screen)

        # Blit the Strip pixel image onto the screen at position (100, 100)
        #grass:
        screen.blit(Grass, (0, 0))
        screen.blit(Grass, (700, 0))
        screen.blit(Grass, (0, 500))
        screen.blit(Grass, (700, 500))
        #controll tower:
        screen.blit(image_tower1, (350, 400))
        screen.blit(image_tower2, (750, 400))
        # corna left
        screen.blit(corna_left, (1400, 0))
        screen.blit(corna_left, (1400, 0))
        screen.blit(Mountain, (1307, 0))
        # strip
        screen.blit(Strip, (1000, 160))
        screen.blit(garag, (1000, 121))
        screen.blit(Strip2, (1000, 500))
        pygame.display.flip()

    pygame.quit()

if __name__ == "__main__":
    main()
