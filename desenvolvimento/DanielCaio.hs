menuAlterarProfessor:: IO ()
menuAlterarProfessor = do
                  putStrLn "Digite o CPF do professor"
                  cpf <- getLine
                  array <- lerProfessores
                  let usuario = buscar (formarProfessor' "a" cpf) array
                  putStrLn ("\t CPF 'cpf' \t1 - Alterar Nome\n\t2 - Alterar salario\n\t3 - Alterar disciplinas\n\t4 - Voltar ao Menu Professor")
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

menuAlterarAluno:: IO ()
menuAlterarAluno = do
                  putStrLn "Digite o CPF do Aluno"
                  cpf <- getLine
                  array <- lerAlunos
                  let usuario = buscar (formarAluno' "a" cpf) array
                  putStrLn ("\t CPF 'cpf' \t1 - Alterar Nome\n\t2 - Alterar disciplinas\n\t3 - Voltar ao Menu Aluno")
                  str <- getLine
                  opcao <- (readIO str)
                  menuExtraAlterarAluno opcao cpf

menuExtraAlterarAluno:: Int -> String -> IO()
menuExtraAlterarAluno 1 cpf = alterarNomeAluno cpf
menuExtraAlterarAluno 2 cpf = alterarDisciplinasAluno cpf
menuExtraAlterarAluno 3 cpf = menuAluno
menuExtraAlterarProfessor _ _= do
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
