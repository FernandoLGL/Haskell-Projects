--Objetivo:
--	-Controle de saida de pratos em um restaurante.
--	-Informar ao Usuario sobre clientes que geram lucro elevado ou reduzido.
--	-Informar ao Usuario Hórarios de melhor funcionamento com relação ao Montate arrecadado naquele periodo tempo


import Prelude hiding (catch)
import Data.Time
import System.IO
import System.IO.Error
import Control.Exception
import System.Directory

--Tipo de data referente ao cliente: o ideal é que ele possua os atributos correspondentes ao nome, cpf, Lucro gerado por ele e a lista de pedidos realizada por ele
--Comparaçoes do cliente: O cliente deve ser igual a outro caso o cpf do mesmo seja igual ao cpf de outro cliente porem eles devem ser ordenados com relação ao lucro gerado pelos mesmos 
data Professor = Professor String String Float String [String] deriving (Show,Read)
instance Eq Professor where
    (==) (Professor _ cpf _ _ _) (Professor _ cpf2 _ _ _) = cpf == cpf2

--Tipo de data referente ao cardapio, o cardapio deve possuir um nome e um preço referente ao mesmo
--Comparaçoes do cardapio: os cardapios devem ser comparados com relação ao seu nome, não há necessidade de organizar-los.
data Aluno = Aluno String String String [String] deriving (Show,Read)
instance Eq Aluno where
    (==) (Aluno _ cpf _ _) (Aluno _ cpf2 _ _) = cpf == cpf2

--Função de inserção: A função de inserção apenas insere um elemento generico em uma lista generica sem se importar com ordem ou com repetição.
inserir :: a->[a]->[a]
inserir a l = a:l

--Função de inserção: A função ordenada de inserção apenas insere um elemento generico em uma lista generica preucupando-se com ordem mas não com a repetição.
inserirOrd ::(Ord a) => a->[a]->[a]
inserirOrd a [] = a:[]
inserirOrd a (h:t) |(a<h) = a:(h:t)
                   |True = h: inserirOrd a t

--Função de remoção: A função de remorção apenas remove todos elementos genericos em uma lista, que são iguais ao programa.
remover ::(Eq a) => a -> [a] -> [a]
remover _ [] = []
remover a (h:t) |(h == a) = remover a t
                |True = h:(remover a t)

--Função de remoção: A função de busca apenas busca um elemento generico em uma lista que é iguais ao pseudo-elemento passado como parametro.
buscar :: (Eq a) => a -> [a] -> a
buscar _ [] = error  "Elemento não encontrado"
buscar a (h:t) |(h == a) = h
               |True = buscar a t

-- Funções de encapsulamanento
-- Função de encpsulamento de Cliente no qual se preucupa apenas com a formação de um novo cliente com relação as informações passadas como parametro no mesmo
formarProfessor nome cpf salario date disciplinas = Professor nome cpf salario date disciplinas

-- Função de encpsulamento de pedidos no qual se preucupa apenas com a formação de um novo pedido com relação as informações passadas como parametro no mesmo

    
-- Função de encpsulamento do cardapio no qual se preucupa apenas com a formação de um novo cardapio com relação as informações passadas como parametro no mesmo


--Função que informa os clientes, em ordem crescente e retorna uma lista no formato  [Cliente (Nome,Cpf) : 10.00, Cliente (Nome,Cpf) : 20.00, Cliente (Nome,Cpf) : 30.00...]: 
--Para isso ela recebe uma lista de clientes, ordena ela(em uma função auxiliar), e por fim usando essa lista ordenada faz uma lista com os clientes com relação as informações requisitadas nesse padrão
--formatListaDeProfessores::[Professor]-> [String]
--formatListaDeProfessores a = formarListaDeProfessores (redoLista a [] )
  -- where  
    --     redoLista:: [Professor] -> [Professor] -> [Professor]
      --   redoLista [] a = a
        -- redoLista (h:t) a = redoLista t (inserirOrd h a)
         --formarListaDeProfessores:: [Professor] -> [String]
         --formarListaDeProfessores [] = []
         --formarListaDeProfessores (((Professor nome cpf salario date disciplinas)):t) = ("Cliente " ++ nome ++" , " ++  cpf ++ " : " ++  show salario ): formatListaDeProfessores t


--Funções de leitura e aramazenamento de arquivos
--Função de armazenamento de uma lista Cliente em disco rigido em um arquivo a ser criado chamado "Clientes.bin"
--salvarAluno :: [Aluno] -> IO()
--salvarAlunos aux = writeFile "Alunos.txt" (show aux)

--Função de armazenamento de um cardapio em disco rigido em um arquivo a ser criado chamado "Cardapio.bin"
salvarProfessores :: [Professor] -> IO()
salvarProfessores aux = writeFile "Professores.txt" (show aux)

--Função de leitura de um cliente com base em um arquivo chamado "Cardapio.bin", essa função tem a possibilidade de lançar 4 exceções as mesmas são checadas e tratadaa
--lerAlunos :: IO ([Aluno])
--lerAlunos = catchIOError (do
--                  str <- readFile "Alunos.txt"
--                  retorno <- (readIO str)
--                  return retorno
--                  )
--                  (funcAux)
--  where funcAux:: IOError -> IO ([Aluno])
--        funcAux e |(isDoesNotExistError e) = return []
--                  |(isPermissionError e) = putStrLn "Por favor verifique as permicoes antes de continuar." >> return []
--                  |(isAlreadyInUseError e) = putStrLn "Feche a aplicação e tente outra vez" >> getLine >>lerAlunos
--                  |(isUserError e) = return []
--                  |True = ioError e   

--Função de leitura de um cliente com base em um arquivo chamado "Cardapio.bin", essa função tem a possibilidade de lançar 4 exceções as mesmas são checadas e tratadaa
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

--Operações interativas
--Menu principal apenas usa dos menus intermediarios para realizar ua operações 
menuPrincipal:: IO ()
menuPrincipal = do
                  putStrLn ("\t1 - Operacoes com o Professor\n\t2 - Operacoes com o Aluno\n\t3 - Sair")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediario opcao

--Menu intermediario que usa do parametro passado para levar o usuario a seu respectivo menu
menuIntermediario:: Int -> IO()
menuIntermediario 1 = menuProfessor
--menuIntermediario 2 = menuAluno
menuIntermediario 3 = return ()
menuIntermediario _ = do
                        putStrLn "erro Tente de novo"
                        menuPrincipal

--Menu referente ao Cliente que leva o usuario a operação requisita
menuProfessor:: IO ()
menuProfessor = do
                  putStrLn ("\t1 - Adicionar\n\t2 - Remover\n\t3 - Alterar\n\t4 - Buscar\n\t5 - Voltar ao Menu Principal")
                  str <- getLine
                  opcao <- (readIO str)
                  menuIntermediarioProfessor opcao

--Menu intermediario que usa do parametro passado para levar o usuario a sua operação requisita 
menuIntermediarioProfessor:: Int -> IO()
menuIntermediarioProfessor 1 = adicionarProfessor
--menuIntermediarioProfessor 2 = removerProfessor
--menuIntermediarioProfessor 3 = alterarProfessor
menuIntermediarioProfessor 4 = listarProfessores
menuIntermediarioProfessor 5 = menuPrincipal
menuIntermediarioProfessor _ = do
                        putStr "opcao invalida"
                        menuProfessor

--Operação de inserção de Cliente interativa que pega o nome e cpf do cliente e usando funções implementadas nesse codigo forma o cliente e adiciona ao arquivo mesmo
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

formarArrayAux [] a = [a]
formarArrayAux (h:t) a = if (h==',') then 
                        a:(formarArrayAux t [])
                   else 
                        formarArrayAux t (a ++ [h])

formarArray s = formarArrayAux s []

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
  

----Operação de remoção de Cliente interativa que pega o  cpf do cliente e usando funções implementadas nesse codigo forma o cliente e remove ao arquivo mesmo, há probilidade de erro portato os mesmos são tratados 
--removerProfessor :: IO()
--removerProfessor = catchIOError (do
  --                      putStrLn "Digite o cpf de quem deseja excluir"
    --                    cpf <- getLine     
      --                  array <- lerProfessores
        --                let newList =  remover (formarProfessor "a" cpf 0 "a" []) array 
          --              salvarProfessores newList 
            --            putStrLn "Operação realizada com sucessso, digite algo para continuar"
              --          getLine
                --        menuProfessor
                  --      )
                    --        (
                      --        funcAux 
                        --    )
  --where
    --funcAux :: IOError -> IO()
    --funcAux  e |(isUserError e) = putStrLn "Erro Cliente não encontrado" >> menuProfessor
      --         |True = ioError e   

----Operação de alteração de Cliente interativa que pega o novo nome e cpf do cliente e usando funções implementadas nesse codigo forma o cliente e remove e adiciona ao arquivo mesmo, há probilidade de erro portato os mesmos são tratados.
--menuAlterarCliente :: IO()
--menuAlterarCliente = catchIOError(do
--                                   putStrLn "Digite o cpf de quem deseja Alterar"
--                                   cpf <- getLine
--                                   putStrLn "Digite o novo nome desse úsuario"
--                                   nome <- getLine
--                                   array <- lerCliente
--                                   let usuario = buscar (formarCliente "a" cpf) array
--                                   let x = ((\(Cliente _ info1 info2 info3) info -> (Cliente info info1 info2 info3)) usuario nome)
--                                   let oldlist = (remover (formarCliente "a" cpf) array) 
--                                   let newList = (inserir x oldlist)
--                                   salvarCliente newList 
--                                   putStrLn "Operação realizada com sucessso, digite algo para continuar"
--                                   getLine
--                                   menuCliente
--                                   )
--                                  (
--                                    funcAux
--                                  )
--  where
--    funcAux :: IOError -> IO()
--    funcAux  e |(isUserError e) = putStrLn "Erro Cliente não encontrado" >> menuCliente
--               |True = ioError e   

----Menu referente ao Cardapio que leva o usuario a operação requisita
--menuCardapio :: IO()
--menuCardapio = do
--                putStrLn "Deseja:\n\t 1-Adiconar um Item ao Cardapio \n\t 2-Remover um Item do Cardapio \n\t 3- Verificar o Cardapio \n 4- retornar ao menu Inicial"
--                str <- getLine
--                opcao <- (readIO str)
--                menuIntermediarioCardapio opcao

----Menu intermediario usa do parametro passado para levar o usuario a sua operação requisita 
--menuIntermediarioCardapio:: Int -> IO()
--menuIntermediarioCardapio 1 = menuAddItem
--menuIntermediarioCardapio 2 = menuRemoverItem
--menuIntermediarioCardapio 3 = menuListarItem
--menuIntermediarioCardapio 4 = menuPrincipal
--menuIntermediarioCardapio _ = do
--                              putStrLn "Erro Tente de novo"
--                              menuCardapio

----Operação de inserção do Caradpio interativa que pega o nome e preço do item e usando funções implementadas nesse codigo forma o item e adiciona ao arquivo mesmo
--menuAddItem:: IO()
--menuAddItem = do
--                   putStrLn "Digite o Nome do Item"
--                   nome <- getLine
--                   putStrLn "Digite o preco do Item"
--                   str <- getLine
--                   preco <- (readIO str)
--                   array <- lerCardapio
--                   let newList = inserir (formarCardapio nome preco) array
--                   salvarCardapio newList 
--                   putStrLn "Operação realizada com sucessso, digite algo para continuar"
--                   getLine
--                   menuCardapio

----Operação de remoção do Caradpio interativa que pega o nome do Caradpio e usando funções implementadas nesse codigo forma o Item e remove ao arquivo mesmo, há probilidade de erro portato os mesmos são tratados 
--menuRemoverItem :: IO()
--menuRemoverItem =catchIOError (do
--                                  putStrLn "Digite o Nome do item que deseja excluir"
--                                  nome <- getLine     
--                                  array <- lerCardapio
--                                  let newList =  remover (formarCardapio nome 0) array
--                                  salvarCardapio newList 
--                                  putStrLn "Operação realizada com sucessso, digite algo para continuar"
--                                  getLine
--                                  menuCardapio
--                              )
--                              (
--                                    funcAux
--                                  )
--  where
--    funcAux :: IOError -> IO()
--    funcAux  e |(isUserError e) = putStrLn "Erro Item não encontrado" >> menuCardapio
--               |True = ioError e   

----Operação de listagem de Cardapio interativa que imprime o cliente com base no arquivo na database
--menuListarItem ::  IO()
--menuListarItem = do
--                      array <- lerCardapio
--                      imprimir array
--                      putStrLn "Digite algo para continuar"
--                      getLine
--                      menuCardapio
--  where
--       imprimir:: [ItensCardapio] -> IO()
--       imprimir [] = return()
--       imprimir ((ItensCardapio nome preco):t) = putStrLn ("item " ++ nome ++ " , preco " ++ show preco) >> imprimir t

----Operação para criação de um pedido para isso funções auxiliares são utilizadas, nessa operação é requisitado o cpf do cliente onde o mesmo é procurado na database e é requisitado os elementos do Caradpio, com essas informações há a requisitação da data que pode ser a do sistema ou informada pelo usuario onde há a validação da mesma e por fim o pedido é adicionad ao cliente que foi selecionado e então é autolizado na database dos clientes.
--menuPedido :: IO()
--menuPedido = catchIOError (do 
--                             putStrLn "Informe o cpf do Cliente que deseja fazer o pedido"
--                             arrayCliente <- lerCliente
--                             cpfCliente <- getLine
--                             let cliente = buscar (formarCliente "" cpfCliente) arrayCliente
--                             putStrLn "Escolha quais Itens deseja, caso deseje mais de um separe-os por espaços"
--                             arrayCardapio <- lerCardapio
--                             imprimirCardapio arrayCardapio
--                             escolhas <- getLine
--                             let itensRequisitados = buscaCardapio (words escolhas) arrayCardapio
--                             putStrLn "Digite 1 caso deseje usar a hora atual do sitema ou 2 caso deseje informar"
--                             str <- getLine
--                             opcao <- (readIO str)
--                             date <- opcaoPedido opcao
--                             let pedido = formarPedido itensRequisitados date
--                             let novoCliente = addAoCliente cliente pedido (recolherPrecos itensRequisitados 0)
--                             let arrayPosRemover = remover cliente arrayCliente
--                             let novoArray = inserir novoCliente arrayPosRemover
--                             salvarCliente novoArray
--                             menuPrincipal)
--                          (funcAux "Erro Cliente ou item não encontrado")
--  where
--         --Função para o Catch
--       funcAux :: String ->  IOError -> IO()
--       funcAux  a e |(isUserError e) = putStrLn a >> menuPrincipal
--                    |True = ioError e   

--       --Função que adiciona o Pedido ao cliente incrementando o lucro gerado por aquele cliente
--       addAoCliente:: Cliente -> Pedidos -> Float -> Cliente
--       addAoCliente (Cliente nome cpf lucro pedidos) h novoLucro = (Cliente nome cpf (lucro+novoLucro) (h:pedidos))

--       --Função que recolhe o preço total do pedido de um Cliente
--       recolherPrecos:: [ItensCardapio] -> Float-> Float
--       recolherPrecos [] a = a
--       recolherPrecos ((ItensCardapio nome preco):t) a = recolherPrecos t (a + preco) 

--       --Função para a impreção do cardapio
--       imprimirCardapio:: [ItensCardapio] -> IO()
--       imprimirCardapio [] = return()
--       imprimirCardapio ((ItensCardapio nome preco):t) = putStrLn ("item " ++ nome ++ " , preco " ++ show preco) >> imprimirCardapio t
       
--       --Função para a busca de n elementos em um cardapio
--       buscaCardapio:: [String] -> [ItensCardapio] -> [ItensCardapio]
--       buscaCardapio [] _ = []
--       buscaCardapio (h:t) a = (buscar (formarCardapio h 0) a) : (buscaCardapio t a)
       
--       --Função que representa a escolha do usuario com relação a hora (1- Deseja a hora atual)/(2- Deseja informar a hora)
--       opcaoPedido :: Int -> IO(((Int,Int,Int),(Int,Int,Int)))
--       opcaoPedido 1 = tempoAtual
--       opcaoPedido 2 = do
--                        putStrLn "Digite a Hora(HH:MM:SS)"
--                        str <- getLine
--                        let (h,m,s) = formarHora str
--                        let hora = ((h+3),m,s)
--                        putStrLn "Digite a Data (YYYY-DD-MM)"
--                        str2 <- getLine
--                        let dia = (formarData str2)
--                        return (hora, dia)
                                      
--       opcaoPedido _ = do
--                          putStrLn " digite 1 caso deseje usar a hora atual do sitema ou 2 caso deseje informar"
--                          str <- getLine
--                          opcao <- (readIO str)
--                          opcaoPedido opcao               
       
--       --Função que, usando da getCurrentTime (Função importada de Date.Time), recupera a hora atual do sistema e forma a que sera usada no pedido
--       tempoAtual :: IO ((Int, Int, Int), (Int, Int, Int))
--       tempoAtual = do
--                date <-getCurrentTime
--                let str = show (date)
--                let hora = formarHora(taking 1 ' ' (taking 0 '.' str))
--                let dia = formarData(taking 0 ' ' str)
--                return (hora,dia)
  
--       --Função para cortar uma string
--       taking :: Int -> Char ->  String -> String
--       taking _ _ [] = []
--       taking 0 parada (h:t)
--                |(h == parada) = []
--                |True = h:taking 0 parada t
--       taking a parada (h:t) 
--                |(h == parada) = taking (a-1) parada t
--                |True = taking a parada t

--       --Função que forma um dia Com base em uma String na formatação (%d-%d-%d) Ano,mes,dia
--       formarData:: String -> (Int, Int, Int) 
--       formarData a  = 
--          let ano = read(taking 0 '-' a)
--              mes = read(taking 1 '-' a)
--              dia = read(taking 2 '-' a)
         
--          in  if mes > 0 && mes <= 12 && ano >= 2015 && (validaDia dia mes ano)  then
--                 (dia,mes,ano)
--              else
--                 error "Numero invalido" 

--       --Função que valida um dia
--       validaDia:: Int -> Int ->Int -> Bool
--       validaDia a b c   |(a <= 0) = False
--                         |(a > 31) = False
--                         |(a == 31 && (b /= 1 && b /= 3 && b/= 5 && b/= 8 && b /=10 && b/= 12 )) = False
--                         --caso de bissexto:
--                         |(a >= 29 && b ==2 && (c `mod` 4 /=0 || ((c `mod` 4) == 0 && (c `mod` 100) /= 0 && (c `mod` 400 == 0)))) =  False
--                         |(a > 29 && b ==2) = False
--                         |(a == 30 && (b /=4 && b /=6 && b /=7 && b /=9 && b /=11)) = False
--                         |True = True 

--       --Função que forma um hora Com base em uma String na formatação (%d:%d:%d) hora,minutos,segundos
--       formarHora:: String -> (Int, Int, Int) 
--       formarHora a  =
--         let hora = read(taking 0 ':' a)
--             minutos = read(taking 1 ':' a)
--             segundos = read(taking 2 ':' a)
--         in  if hora <= 23 && hora >= 0 && minutos >= 0 && minutos <= 59 && segundos >= 0 && segundos <= 59 then
                   
--                   (transhora (hora-3),minutos,segundos)
--               else
--                 error "Numero invalido"
--       -- Conversor de Horas para o Horário brasileiro
--       transhora:: Int -> Int
--       transhora  a |a<0 = a+24
--                    |True = a

----Função para imprimir a informação para o melhor horario de funcionamento
--fomatedHour :: IO()
--fomatedHour= do
--              array <- lerCliente
--              imprimir (inverte (horarios array) []) 1
--              getLine
--              menuPrincipal
--  where
--  	--Função para inverter uma Lista necessaria por causa do Método Horarios
--    inverte:: [a] -> [a] -> [a]
--    inverte [] a = a 
--    inverte (h:t) a = inverte t ([h]++a)
--    --Função para imprimir as horas dado informações adicionais ao usuari
--    imprimir::[(Float,Int)] -> Int -> IO()
--    imprimir [] _ = do
--                     putStrLn "Não há lucro computado em outros horarios"
--                     return ()
--    imprimir ((lucro,horario):t) a |(a==1) = putStrLn ("O melhor horario e as "++ show horario ++" horas, ele gera um lucro de " ++ show lucro) >> imprimir t (a+1)
--                                   |(a==2) = putStrLn ("O segundo melhor horario e as "++ show horario ++" horas, ele gera um lucro de " ++ show lucro)>> imprimir t (a+1)
--                                   |(a==3) = putStrLn ("O terceiro melhor horario e as "++ show horario ++" horas, ele gera um lucro de " ++ show lucro)>> imprimir t (a+1)
--                                   |True = putStrLn ("O "++ show a ++"º melhor horario e as "++ show horario ++" horas, ele gera um lucro de " ++ show lucro)>> imprimir t (a+1)

--Função principal que apenas chama as outras. Importante para o momento de compilação
main:: IO()
main = menuPrincipal