##
## EPITECH PROJECT, 2019
## makefile
## File description:
## imageCompressor Makefile
##

NAME		=	imageCompressor

STACK		=	stack

DIR_EXE		=	$(shell stack path --local-install-root)

all: $(NAME)

$(NAME):
	$(STACK) build
	cp $(DIR_EXE)/bin/$(NAME)-exe ./$(NAME)

clean:
	$(STACK) clean

fclean:
	$(RM) $(NAME)
	$(STACK) clean --full

re: fclean all

.PHONY: all clean fclean re
