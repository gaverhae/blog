import pygame

pygame.init()

width = 600
height = 600

display = pygame.display.set_mode((width, height))
pygame.display.set_caption("Drawing Objects")

black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)
blue = (0, 0, 255)
yellow = (255, 255, 0)
cyan = (0, 255, 255)
magenta = (255, 0, 255)

display.fill(blue)
pygame.draw.line(display, red, (0, 0), (100, 100), 5)
pygame.draw.line(display, green, (100, 100), (200, 300), 1)
pygame.draw.circle(display, white, (width//2, height//2), 200, 6)
pygame.draw.circle(display, yellow, (width//2, height//2), 195, 0)
pygame.draw.rect(display, cyan, (500, 0, 100, 100))
pygame.draw.rect(display, magenta, (500, 100, 50, 100))

running = True

while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
    pygame.display.update()

pygame.quit()
