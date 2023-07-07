import pygame

width = 600
height = 300

black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)
blue = (0, 0, 255)
yellow = (255, 255, 0)
cyan = (0, 255, 255)
magenta = (255, 0, 255)

def draw_shapes(disp):
    disp.fill(blue)
    pygame.draw.line(disp, red, (0, 0), (100, 100), 5)
    pygame.draw.line(disp, green, (100, 100), (200, 300), 1)
    pygame.draw.circle(disp, white, (width//2, height//2), 200, 6)
    pygame.draw.circle(disp, yellow, (width//2, height//2), 195, 0)
    pygame.draw.rect(disp, cyan, (500, 0, 100, 100))
    pygame.draw.rect(disp, magenta, (500, 100, 50, 100))

dragon_left = pygame.image.load("assets/dragon_left.png")
dragon_right = pygame.image.load("assets/dragon_right.png")

def draw_dragons(disp):
    dragon_left_rect = dragon_left.get_rect()
    dragon_left_rect.topleft = (0, 0)
    dragon_right_rect = dragon_right.get_rect()
    dragon_right_rect.topright = (width, 0)
    disp.blit(dragon_left, dragon_left_rect)
    disp.blit(dragon_right, dragon_right_rect)
    pygame.draw.line(disp, white, (0, 75), (width, 75), 4)

pygame.init()

display = pygame.display.set_mode((width, height))
pygame.display.set_caption("Blitting Images!")

running = True

while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
    draw_dragons(display)
    pygame.display.update()

pygame.quit()
