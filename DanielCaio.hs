alterarProfessor :: IO()
alterarProfessor = do
                        putStrLn "Digite o CPF do professor"
                        cpf <- getLine
                        putStrLn "Altere o nome do professor(se não quiser digite o nome antigo)"
                        nome <- getLine
                        putStrLn "Altere o salario do professor(se não quiser digite o salario antigo)"
                        str <- getLine
                        salario <- (readIO str) 
                        putStrLn "Altere as disciplinas que ele leciona separadas por virgula(se não quiser digite as disciplinas antigas)"
                        disciplinas <- getLine
                        let lista = formarArray disciplinas
                        array <- lerProfessores
                        let usuario = buscar (formarProfessor' "a" cpf) array
                        let x = ((\(Professor _ info1 _ info3 _ ) info info2 info4 -> (Professor info info1 info2 info3 info4)) usuario nome salario lista)
                        let listavelha = (remover (formarProfessor' "a" cpf) array) 
                        let listanova = (inserir x listavelha)
                        salvarProfessores listanova 
                        putStrLn "Operação realizada com sucesso. Digite algo para prosseguir."
                        getLine
                        menuProfessor
