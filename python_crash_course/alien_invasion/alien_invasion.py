import sys
import pygame
from settings import Settings
from ship import Ship

class AlienInvasion:
    """Classe geral para gerenciar ativos e comportamentos do jogo"""

    def __init__(self):
        #Inicializa o jogo e cria os recursos do jogo
        pygame.init()
        self.clock = pygame.time.Clock()
        self.settings = Settings()

        self.screen = pygame.display.set_mode((self.settings.screen_width, self.settings.screen_height))
        pygame.display.set_caption("Alien Invasion")

        self.ship = Ship(self)


    def run_game(self):
        """Inicia o loop principal do jogo"""

        while True:
            #Observa eventos de teclado e mouse
            self._check_events()
            self._update_screen()
            self.clock.tick(60)


    def _check_events(self):

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    sys.exit()


    def _update_screen(self):
            #Redesenha a tela durante cada passagem pelo loop
            self.screen.fill(self.settings.bg_color)
            self.ship.blitme()

            pygame.display.flip()
            #Deixa a tela desenhada mais recente visível




#Como esse é o jogo principal, chama o método run_game()
# por isso está fora da classe --> AlienInvasion
if __name__ == '__main__':
#Cria uma instância e chama o método run_game()
    ai = AlienInvasion()
    ai.run_game()














    