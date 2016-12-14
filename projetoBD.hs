--Alunos: Daniel Vasconcellos, Fernando Rosendo, Eduardo Albuquerque

import Prelude hiding (catch)
import Data.Time
import System.IO
import System.IO.Error
import Control.Exception
import System.Directory

--Tipo de data que se refere ao professor, tem um nome, cpf, salário, data que ingressou, disciplina(s) que ensina 
data Professor = Professor String String Float String [Disciplina] deriving (Show,Read)
instance Eq Professor where
    (==) (Professor _ cpf _ _ _) (Professor _ cpf2 _ _ _) = cpf == cpf2

--Tipo de data que se refere ao aluno, possui um nome, cpf, data que ingressou e disciplinas que cursa
data Aluno = Aluno String String String [Disciplina]  deriving (Show,Read)
instance Eq Aluno where
    (==) (Aluno _ cpf _ _) (Aluno _ cpf2 _ _) = cpf == cpf2

--funcao para insercao
inserir a l = a:l

--funcao para remocao
remover ::(Eq a) => a -> [a] -> [a]
remover _ [] = []
remover a (h:t) |(h == a) = remover a t
                |True = h:(remover a t)

--funcao para busca
buscar _ [] = error  "Elemento não encontrado"
buscar a (h:t) |(h == a) = h
               |True = buscar a t

--salvar alunos num arquivo de texto
salvarAlunos aux = writeFile "Alunos.txt" (show aux)

--salvar professores num arquivo de texto
salvarProfessores aux = writeFile "Professores.txt" (show aux)

--funcao para ler o arquivo de alunos
lerAlunos :: IO ([Aluno])
lerAlunos = catchIOError (do
                  str <- readFile "Alunos.txt"
                  retorno <- (readIO str)
                  return retorno
                  )
                  (funcAux)
  where funcAux:: IOError -> IO ([Aluno])
        funcAux e |(isDoesNotExistError e) = return []
                  |(isPermissionError e) = putStrLn "Por favor verifique as permicoes antes de continuar." >> return []
                  |(isAlreadyInUseError e) = putStrLn "Feche a aplicação e tente outra vez" >> getLine >>lerAlunos
                  |(isUserError e) = return []
                  |True = ioError e

--funcao para ler o arquivo de professores
lerProfessores :: IO ([Professor])
lerProfessores = catchIOError (do
                  str <- readFile "Professores.txt"
                  retorno <- (readIO str)
                  return retorno
                  )
                  (funcAux)
  where funcAux:: IOError -> IO ([Professor])
        funcAux e |(isDoesNotExistError e) = return []
                  |(isPermissionError e) = putStrLn "Por favor verifique as permicoes antes de continuar." >> return []
                  |(isAlreadyInUseError e) = putStrLn "Feche a aplicação e tente outra vez" >> getLine >>lerProfessores
                  |(isUserError e) = return []
                  |True = ioError e

formarArrayAux [] a = [a]
formarArrayAux (h:t) a = if (h==',') then 
                        a:(formarArrayAux t [])
                   else 
                        formarArrayAux t (a ++ [h])

formarArray s = formarArrayAux s []


--formar um professor
formarProfessor nome cpf salario date disciplinas = Professor nome cpf salario date disciplinas

--menu principal
menuPrincipal:: IO ()
menuPrincipal = do
                  putStrLn ("\t1 - Operacoes com o Professor\n\t2 - Operacoes com o Aluno\n\t3 - Sair")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediario opcao

--menu intermediario
menuIntermediario:: Int -> IO()
menuIntermediario 1 = menuProfessor
menuIntermediario 2 = menuAluno
menuIntermediario 3 = return ()
menuIntermediario _ = do
                        putStrLn "erro Tente de novo"
                        menuPrincipal

-- menu do professor
menuProfessor:: IO ()
menuProfessor = do
                  putStrLn ("\t1 - Adicionar\n\t2 - Remover\n\t3 - Alterar\n\t4 - Buscar\n\t5 - Voltar ao Menu Principal")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediarioProfessor opcao

--menu intermediario do professor
menuIntermediarioProfessor:: IO()
menuIntermediarioProfessor 1 = adicionarProfessor
menuIntermediarioProfessor 2 = removerProfessor
menuIntermediarioProfessor 3 = alterarProfessor
menuIntermediarioProfessor 4 = buscarProfessor
menuIntermediarioProfessor 5 = menuPrincipal
menuIntermediarioProfessor _ = do
                        putStr "opcao invalida"
                        menuProfessor

--funcao para adicionar professor
adicionarProfessor :: IO()
adicionarProfessor = do
                        putStrLn "Digite o nome do professor"
                        nome <- getLine
                        putStrLn "Digite o CPF do professor"
                        cpf <- getLine
                        putStrLn "Digite o salario do professor"
                        str <- getLine
                        salario <- (readIO str) 
                        putStrLn "Digite a data de ingressao do professor"
                        date <- getLine
                        putStrLn "Digite as disciplinas que ele leciona separadas por virgula"
                        disciplinas <- getLine
                        let lista = formarArray disciplinas
                        jaExistentes <- lerProfessores
                        let profSalvar = inserir (formarProfessor nome cpf salario date lista) jaExistentes
                        salvarProfessores profSalvar
                        putStrLn "Operacao realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuProfessor

--funçao para remover professor
removerProfessor :: IO()
removerProfessor = catchIOError (do
                        putStrLn "Digite o cpf de quem deseja excluir"
                        cpf <- getLine     
                        array <- lerProfessores
                        let newList =  remover (formarProfessor "a" cpf 0 "a" []) array 
                        salvarProfessores newList 
                        putStrLn "Operação realizada com sucessso, digite algo para continuar"
                        getLine
                        menuProfessor
                        )
                            (
                              funcAux 
                            )
  where
    funcAux :: IOError -> IO()
    funcAux  e |(isUserError e) = putStrLn "Erro Cliente não encontrado" >> menuProfessor
               |True = ioError e                          


-- menu do aluno
menuAluno:: IO ()
menuAluno = do
                  putStrLn ("\t1 - Adicionar\n\t2 - Remover\n\t3 - Alterar\n\t4 - Buscar\n\t5 - Voltar ao Menu Principal")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediarioAluno opcao

--menu intermediario do aluno
menuIntermediarioAluno:: IO()
menuIntermediarioAluno 1 = adicionarAluno
menuIntermediarioAluno 2 = removerAluno
menuIntermediarioAluno 3 = alterarAluno
menuIntermediarioAluno 4 = buscarAluno
menuIntermediarioAluno 5 = menuPrincipal
menuIntermediarioAluno _ = do
                        putStr "opcao invalida"
                        menuAluno

--Função principal que apenas chama as outras. Importante para o momento de compilação (iniciar o codigo)
main:: IO()
main = menuPrincipal