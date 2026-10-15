import pygame

class Ship:
    #classe para cuidar da espaçonave
    def __init__(self, ai_game):
        #inicializa o objeto da nave e define sua posição inicial
        self.screen = ai_game.screen
        self.screen_rect = ai_game.screen.get_rect()

        #carrega a imagem da nave e obtem seu rect
        self.image = pygame.image.load('images/ship.bmp')
        self.rect = self.image.get_rect()

        #inicia cada nave na parte inferior central da tela
        self.rect.midbottom = self.screen_rect.midbottom

    def blitme(self):
        #desenha a nave na sua posição atual
        self.screen.blit(self.image, self.rect)
