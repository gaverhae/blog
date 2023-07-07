import pygame

pygame.init()

width = 600
height = 300

display = pygame.display.set_mode((width, height))
pygame.display.set_caption("Hello World!")

running = True

while running:
    for event in pygame.event.get():
        print(event)
        if event.type == pygame.QUIT:
            running = False

pygame.quit()
