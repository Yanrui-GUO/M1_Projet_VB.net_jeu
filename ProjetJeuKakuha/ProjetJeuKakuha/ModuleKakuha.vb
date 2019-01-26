Module ModuleKakuha

    Public table(12) As Integer
    Public graines As Integer
    Public joueur As Integer
    Public fois As Integer 'ça veut dire le fois maximun qu'on peut semer selon le nombre de graines à la main.
    Public score(2) As Integer
    Public nonvide As Boolean 'si le trou suivant n'est pas vide, nonvide est vrai. 




    Public Sub Jouer(ByVal i As Integer)
        'par defaut, le joueur 1 peut commencer avance.
        'i : numéro de trou

        Dim n As Integer 'indice de fois qu'on peut semer


        graines = table(i) 'c'est le nombre de graines dans le trou qu'on choisi.
        fois = graines - 1 'On sais qu'il faut garder la dernier graine, donc on déduit une graine au début.
        table(i) = 0
        'Au premier cas, on suppose que le numéro du trou dernier qu'on sème (= i + fois) est inférieure ou égale à 12. 
        'On vide le trou qu'on choisi et on sème les graines une à une jusqu'à l'avant dernière.
        If i + fois <= 12 Then
            For n = 1 To fois

                table(i + n) = table(i + n) + 1
            Next

        ElseIf i + fois <= 24 Then

            'Quand le numéro du trou dernier qu'on sème est supérieure à 12, il faut recommencer à partir du numéro 1.

            'Par exemple, supposons que i = 10, graines = 5, fois = 4, donc (i + graines - 1) = 14 est bien supérieure à 12. 
            'En effet, on doit semer les graines à trous 11,12 et 1,2

            'Donc on divise ce cas en 3 parties. 
            'Partie 1 : La somme du numéro de trou et le numéro de graines dans le trou est inférieure à 12. 
            'ici c'est l'évolution de nombre de graine dans le trou 1 et 2, parce qu'on doit passer la rangée de "7-12" à "1-6", autrement dire, la deuxième tour.

            'pour recommencer à partir du numéro 1, on doit savoir qu'on doit semer combien de fois au trou ce qui est sur la rangée "1-6"
            For n = 1 To i + fois - 12 '(i + fois - 12) est le numéro du trou dernier qu'on sème à la nouvelle rangée. Egalement, ça egale le fois de sème car cette rangée commence à 1. Dans ce exemple, Pour n = 1 à 2 
                'Dans l'exemple, Table(10) = 0, c-à-d qu'on vide le trou qu'on choisi
                table(n) = table(n) + 1       'On considère n comme le fois et le numéro en même temp. Plus précisément, ici, il rest 2 fois de sème : prémiere fois est dans le trou(1), deuxième fois est dans le trou(2). 
            Next

            'Patie 2 : le coup de jouer va retourner au premier trou.
            'ici c'est l'évolution de nombre de graine dans le trou 11 et 12
            For n = 1 To 12 - i '(12-i) est pour savoir qu'il y a combien de fois de sème entre trou(i) et trou(12).

                table(i + n) = table(i + n) + 1
            Next
        Else

            'Partie 3: Si i + g - 1 > 24, le coup de jouer va faire 2 retour.
            For n = 1 To 12 - i
                table(i + n) = table(i + n) + 1
            Next

            For n = 1 To 12
                table(n) = table(n) + 1
            Next

            For n = 1 To fois - 12 - (12 - i)
                table(n) = table(n) + 1
            Next

        End If

        'On met à jour des nouveaux nombres des graines dans les trous.
        score(joueur) = score(joueur) + 1



    End Sub

    Public Function PeutJouer(ByVal i As Integer) As Boolean
        'Si on n'a plus de graines sur la main et va prendre les graines au trou prochianes. Du coup, avant passer l'étape prochaine on verifie le nombre de graines du trou suivant.
        'Si le trou suivant est 0, le joueur s'arrête ici et on change le joueur.
        'i est le numéro de trou
        'Dans ce function on utilise 'Joueur' qu'on le défini au début de module


        nonvide = True  'On suppose qu'il exist des graines dans le trou dans lequel il aurait dû prendre des graines.

        fois = graines - 1 'On sais qu'il faut garder la dernier graine, donc on déduit une graine au début.


        If i + fois < 12 Then 'Cas 1:
            If table(i + fois + 1) = 0 Then
                nonvide = False
                'Le trou suivant est vide
                If joueur = 1 Then
                    'Dan ce cas, si les trous de Joueur 2 est tous vide, on n'a pas besoin de changer le joueur.  C'est encore joueur 1 qui a la main.
                    If table(7) <> 0 Or table(8) <> 0 Or table(9) <> 0 Or table(10) <> 0 Or table(11) <> 0 Or table(12) <> 0 Then

                        joueur = 3 - joueur 'C'est pour changer le joueur. Par exemple, joueur 1 s'arrêt ici, numéro du joueur = 3 - 1 = 2, c'est-à-dire le joueur 2 a la main.

                    End If
                Else
                    If table(1) <> 0 Or table(2) <> 0 Or table(3) <> 0 Or table(4) <> 0 Or table(5) <> 0 Or table(6) <> 0 Then

                        joueur = 3 - joueur

                    End If
                End If

            End If

        ElseIf i + fois < 24 And i + fois >= 12 Then 'Cas 2:
            If table(i + fois + 1 - 12) = 0 Then
                nonvide = False
                If joueur = 1 Then
                    If table(7) <> 0 Or table(8) <> 0 Or table(9) <> 0 Or table(10) <> 0 Or table(11) <> 0 Or table(12) <> 0 Then

                        joueur = 3 - joueur
                    End If
                Else
                    If table(1) <> 0 Or table(2) <> 0 Or table(3) <> 0 Or table(4) <> 0 Or table(5) <> 0 Or table(6) <> 0 Then

                        joueur = 3 - joueur
                    End If
                End If
            End If

        ElseIf i + fois >= 24 Then 'Cas 3:
            If table(i + fois - 24 + 1) = 0 Then
                nonvide = False
                If joueur = 1 Then
                    If table(7) <> 0 Or table(8) <> 0 Or table(9) <> 0 Or table(10) <> 0 Or table(11) <> 0 Or table(12) <> 0 Then

                        joueur = 3 - joueur
                    End If
                Else
                    If table(1) <> 0 Or table(2) <> 0 Or table(3) <> 0 Or table(4) <> 0 Or table(5) <> 0 Or table(6) <> 0 Then

                        joueur = 3 - joueur
                    End If
                End If
            End If
        End If
        Return nonvide



    End Function

    Public Function JeuTermine(ByVal joueur As Integer) As Boolean

        If score(joueur) > 24 Or score(1) = score(2) = 24 Then 'Si un joueur garde plus la moitié des graines, ce joueur a gagné. Si 2 joueurs ont la même nombre de graines de 24, c'est un match nul.
            JeuTermine = True
        Else
            JeuTermine = False
        End If

        Return JeuTermine

    End Function

End Module
