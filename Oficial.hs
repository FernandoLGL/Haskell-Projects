--Alunos: Daniel Vasconcellos, Fernando Rosendo, Eduardo Albuquerque

import Prelude hiding (catch)
import Data.Time
import System.IO
import System.IO.Error
import Control.Exception
import System.Directory

--Tipo de data que se refere ao professor, tem um nome, cpf, salário, data que ingressou, disciplina(s) que ensina 
data Professor = Professor String String Float String [String] deriving (Show,Read)
instance Eq Professor where
    (==) (Professor _ cpf _ _ _) (Professor _ cpf2 _ _ _) = cpf == cpf2

--Tipo de data que se refere ao aluno, possui um nome, cpf, data que ingressou e disciplinas que cursa
data Aluno = Aluno String String String [String]  deriving (Show,Read)
instance Eq Aluno where
    (==) (Aluno _ cpf _ _) (Aluno _ cpf2 _ _) = cpf == cpf2

--Função de inserção: A função de inserção apenas insere um elemento generico em uma lista generica sem se importar com ordem ou com repetição.
inserir :: a->[a]->[a]
inserir a l = a:l

--funcao para remocao
remover ::(Eq a) => a -> [a] -> [a]
remover _ [] = []
remover a (h:t) |(h == a) = remover a t
                |True = h:(remover a t)

--funcao para busca
--busca um elemento generico em uma lista que é igual ao elemento passado como parametro.
buscar :: (Eq a) => a -> [a] -> a
buscar _ [] = error "Elemento não encontrado"
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

--formar um aluno(encapsulamento)
formarAluno nome cpf date disciplinas = Aluno nome cpf date disciplinas

--alternativa para formar um aluno(encapsulamento)
formarAluno' nome cpf = Aluno nome cpf "" []


--formar um professor(encapsulamento)
formarProfessor nome cpf salario date disciplinas = Professor nome cpf salario date disciplinas

--alternativa para formar um professor
formarProfessor' nome cpf = Professor nome cpf 0 "" []

--menu principal
menuPrincipal:: IO ()
menuPrincipal = do
                  putStrLn ("\n\t---MENU PRINCIPAL---\n\n\t1 - Operacoes com o Professor\n\t2 - Operacoes com o Aluno\n\t3 - Créditos\n\t4 - Sair")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediario opcao

--menu intermediario
menuIntermediario:: Int -> IO()
menuIntermediario 1 = menuProfessor
menuIntermediario 2 = menuAluno
menuIntermediario 3 = credits
menuIntermediario 4 = return ()
menuIntermediario _ = do
                        putStrLn "erro Tente de novo"
                        menuPrincipal

-- menu do professor
menuProfessor:: IO ()
menuProfessor = do
                  putStrLn ("\n\t---MENU PROFESSOR---\n\n\t1 - Adicionar\n\t2 - Remover\n\t3 - Alterar\n\t4 - Listar\n\t5 - Voltar ao Menu Principal")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediarioProfessor opcao

--menu intermediario do professor
menuIntermediarioProfessor:: Int -> IO()
menuIntermediarioProfessor 1 = adicionarProfessor
menuIntermediarioProfessor 2 = removerProfessor
menuIntermediarioProfessor 3 = menuAlterarProfessor
menuIntermediarioProfessor 4 = listarProfessores
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
                        putStrLn "Operacao realizada com sucessso. Digite algo para prosseguir."
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

--funcao para alterar um professor cadastrado
menuAlterarProfessor:: IO ()
menuAlterarProfessor = do
                  putStrLn "Digite o CPF do professor"
                  cpf <- getLine
                  array <- lerProfessores
                  let usuario = buscar (formarProfessor' "a" cpf) array
                  putStrLn ("\t1 - Alterar Nome\n\t2 - Alterar salario\n\t3 - Alterar disciplinas\n\t4 - Voltar ao Menu Professor")
                  str <- getLine
                  opcao <- (readIO str)
                  menuExtraAlterarProfessor opcao cpf

menuExtraAlterarProfessor:: Int -> String -> IO()
menuExtraAlterarProfessor 1 cpf = alterarNomeProf cpf
menuExtraAlterarProfessor 2 cpf = alterarSalarioProf cpf
menuExtraAlterarProfessor 3 cpf = alterarDisciplinasProf cpf
menuExtraAlterarProfessor 4 cpf = menuProfessor
menuExtraAlterarProfessor _ _= do
                        putStr "opcao invalida"
                        menuAlterarProfessor


alterarNomeProf:: String -> IO()
alterarNomeProf cpf = do
                        putStrLn "Altere o nome do professor"
                        nome <- getLine
                        array <- lerProfessores
                        let usuario = buscar (formarProfessor' "a" cpf) array
                        let x = ((\(Professor _ info1 info2 info3 info4) info  -> (Professor info info1 info2 info3 info4)) usuario nome)
                        let listavelha = (remover (formarProfessor' "a" cpf) array) 
                        let listanova = (inserir x listavelha)
                        salvarProfessores listanova 
                        putStrLn "Operação realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuProfessor

alterarSalarioProf:: String -> IO()
alterarSalarioProf cpf = do
                        putStrLn "Altere o salario do professor"
                        str <- getLine
                        salario <- (readIO str)
                        array <- lerProfessores
                        let usuario = buscar (formarProfessor' "a" cpf) array
                        let x = ((\(Professor info info1 _ info3 info4) info2  -> (Professor info info1 info2 info3 info4)) usuario salario)
                        let listavelha = (remover (formarProfessor' "a" cpf) array) 
                        let listanova = (inserir x listavelha)
                        salvarProfessores listanova 
                        putStrLn "Operação realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuProfessor

alterarDisciplinasProf:: String -> IO()
alterarDisciplinasProf cpf = do
                        putStrLn "Altere as disciplinas que ele leciona separadas por virgula"
                        disciplinas <- getLine
                        let lista = formarArray disciplinas
                        array <- lerProfessores
                        let usuario = buscar (formarProfessor' "a" cpf) array
                        let x = ((\(Professor info info1 info2 info3 _ ) info4  -> (Professor info info1 info2 info3 info4)) usuario lista)
                        let listavelha = (remover (formarProfessor' "a" cpf) array) 
                        let listanova = (inserir x listavelha)
                        salvarProfessores listanova 
                        putStrLn "Operação realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuProfessor

--Funçao para listar todos os professores cadastrados no banco de dados
listarProfessores :: IO()
listarProfessores = do  
                      array <- lerProfessores
                      imprimir (formatListaDeProfessor array)
                      putStrLn "Digite algo para continuar"
                      getLine
                      menuProfessor

    where
       imprimir:: [String] -> IO()
       imprimir [] = return()
       imprimir (h:t) = putStrLn h >> imprimir t

formatListaDeProfessor::[Professor]-> [String]
formatListaDeProfessor a = formarListaDeProfessor (redoLista a [] )
   where  
         redoLista:: [Professor] -> [Professor] -> [Professor]
         redoLista [] a = a
         redoLista (h:t) a = redoLista t (inserir h a)
         formarListaDeProfessor:: [Professor] -> [String]
         formarListaDeProfessor [] = []
         formarListaDeProfessor (((Professor nome cpf salario date disciplina)):t) = ("PROFESSOR: " ++ nome ++" CPF: " ++  cpf ++ " SALARIO: " ++ show salario ++ " DATA DE INGRESSO: " ++ date ++ " DISCIPLINAS: " ++ show disciplina ): formatListaDeProfessor t       



-- menu do aluno
menuAluno:: IO ()
menuAluno = do
                  putStrLn ("\n\t---MENU ALUNO---\n\n\t1 - Adicionar\n\t2 - Remover\n\t3 - Alterar\n\t4 - Listar\n\t5 - Voltar ao Menu Principal")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediarioAluno opcao

--menu intermediario do aluno
menuIntermediarioAluno:: Int -> IO()
menuIntermediarioAluno 1 = adicionarAluno
menuIntermediarioAluno 2 = removerAluno
menuIntermediarioAluno 3 = menuAlterarAluno
menuIntermediarioAluno 4 = listarAlunos
menuIntermediarioAluno 5 = menuPrincipal
menuIntermediarioAluno _ = do
                        putStr "opcao invalida"
                        menuAluno

--funcao para adicionar professor
adicionarAluno :: IO()
adicionarAluno = do
                        putStrLn "Digite o nome do Aluno"
                        nome <- getLine
                        putStrLn "Digite o CPF do Aluno"
                        cpf <- getLine
                        putStrLn "Digite a data de ingresso do Aluno"
                        date <- getLine
                        putStrLn "Digite as disciplinas que ele cursa separadas por virgula"
                        disciplinas <- getLine
                        let lista = formarArray disciplinas
                        jaExistentes <- lerAlunos
                        let alunoSalvar = inserir (formarAluno nome cpf date lista) jaExistentes
                        salvarAlunos alunoSalvar
                        putStrLn "Operacao realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuAluno

--funçao para remover professor
removerAluno :: IO()
removerAluno = catchIOError (do
                        putStrLn "Digite o cpf de quem deseja excluir"
                        cpf <- getLine     
                        array <- lerAlunos
                        let newList =  remover (formarAluno "a" cpf "a" []) array 
                        salvarAlunos newList 
                        putStrLn "Operacao realizada com sucessso. Digite algo para prosseguir."
                        getLine
                        menuAluno
                        )
                            (
                              funcAux 
                            )
  where
    funcAux :: IOError -> IO()
    funcAux  e |(isUserError e) = putStrLn "Erro Aluno não encontrado" >> menuAluno
               |True = ioError e                             


menuAlterarAluno:: IO ()
menuAlterarAluno = do
                  putStrLn "Digite o CPF do Aluno"
                  cpf <- getLine
                  array <- lerAlunos
                  let usuario = buscar (formarAluno' "a" cpf) array
                  putStrLn ("\t1 - Alterar Nome\n\t2 - Alterar disciplinas\n\t3 - Voltar ao Menu Aluno")
                  str <- getLine
                  opcao <- (readIO str)
                  menuExtraAlterarAluno opcao cpf

menuExtraAlterarAluno:: Int -> String -> IO()
menuExtraAlterarAluno 1 cpf = alterarNomeAluno cpf
menuExtraAlterarAluno 2 cpf = alterarDisciplinasAluno cpf
menuExtraAlterarAluno 3 cpf = menuAluno
menuExtraAlterarAluno _ _= do
                        putStr "opcao invalida"
                        menuAlterarAluno

alterarNomeAluno:: String -> IO()
alterarNomeAluno cpf = do
                        putStrLn "Altere o nome do Aluno"
                        nome <- getLine
                        array <- lerAlunos
                        let usuario = buscar (formarAluno' "a" cpf) array
                        let x = ((\(Aluno _ info1 info2 info3 ) info  -> (Aluno info info1 info2 info3 )) usuario nome)
                        let listavelha = (remover (formarAluno' "a" cpf) array) 
                        let listanova = (inserir x listavelha)
                        salvarAlunos listanova 
                        putStrLn "Operação realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuAluno

alterarDisciplinasAluno:: String -> IO()
alterarDisciplinasAluno cpf = do
                        putStrLn "Altere as disciplinas que ele esta pagando separadas por virgula"
                        disciplinas <- getLine
                        let lista = formarArray disciplinas
                        array <- lerAlunos
                        let usuario = buscar (formarAluno' "a" cpf) array
                        let x = ((\(Aluno info info1 info2 _ ) info3  -> (Aluno info info1 info2 info3)) usuario lista)
                        let listavelha = (remover (formarAluno' "a" cpf) array) 
                        let listanova = (inserir x listavelha)
                        salvarAlunos listanova 
                        putStrLn "Operação realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuAluno

--Funçao para listar todos os professores cadastrados no banco de dados
listarAlunos :: IO()
listarAlunos = do  
                      array <- lerAlunos
                      imprimir (formatListaDeAluno array)
                      putStrLn "Digite algo para continuar"
                      getLine
                      menuAluno

    where
       imprimir:: [String] -> IO()
       imprimir [] = return()
       imprimir (h:t) = putStrLn h >> imprimir t

formatListaDeAluno::[Aluno]-> [String]
formatListaDeAluno a = formarListaDeAluno (redoLista a [] )
   where  
         redoLista:: [Aluno] -> [Aluno] -> [Aluno]
         redoLista [] a = a
         redoLista (h:t) a = redoLista t (inserir h a)
         formarListaDeAluno:: [Aluno] -> [String]
         formarListaDeAluno [] = []
         formarListaDeAluno (((Aluno nome cpf date disciplina)):t) = ("ALUNO:: " ++ nome ++" CPF: " ++  cpf ++ " DATA DE INGRESSO: " ++ date ++ " DISCIPLINAS: " ++ show disciplina ): formatListaDeAluno t       

credits :: IO()
credits = do
                        putStrLn "Desenvolvido por: Daniel Vasconcellos, Eduardo Müller e Fernando Rosendo"
                        getLine
                        menuPrincipal
--Função principal que apenas chama as outras. Importante para o momento de compilação (iniciar o codigo)
main:: IO()
main = menuPrincipal
