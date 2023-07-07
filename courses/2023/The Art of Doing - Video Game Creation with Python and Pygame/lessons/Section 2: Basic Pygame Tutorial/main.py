import pygame

pygame.init()

dragon_left = pygame.image.load("assets/dragon_left.png")
dragon_right = pygame.image.load("assets/dragon_right.png")
system_font = pygame.font.SysFont('calibri', 64)
custom_font = pygame.font.Font('assets/AttackGraffiti.ttf', 32)

width = 600
height = 300

black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)
dark_green = (10, 50, 10)
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

def draw_dragons(disp):
    dragon_left_rect = dragon_left.get_rect()
    dragon_left_rect.topleft = (0, 0)
    dragon_right_rect = dragon_right.get_rect()
    dragon_right_rect.topright = (width, 0)
    disp.blit(dragon_left, dragon_left_rect)
    disp.blit(dragon_right, dragon_right_rect)
    pygame.draw.line(disp, white, (0, 75), (width, 75), 4)

def render_text(disp):
    sys_t = system_font.render("Dragons Rule!", True, green, dark_green)
    sys_tr = sys_t.get_rect()
    sys_tr.center = (width // 2, height // 2)
    cus_t = custom_font.render("Move the dragon soon!", True, green)
    cus_tr = cus_t.get_rect()
    cus_tr.center = (width // 2, height // 2 + 100)
    disp.blit(sys_t, sys_tr)
    disp.blit(cus_t, cus_tr)

display = pygame.display.set_mode((width, height))
pygame.display.set_caption("Blitting Text!")

running = True

while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
    render_text(display)
    pygame.display.update()

pygame.quit()
