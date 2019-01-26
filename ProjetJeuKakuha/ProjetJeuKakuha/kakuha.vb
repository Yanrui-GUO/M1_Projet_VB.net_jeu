Public Class kakuha
    Public button(12) As Button
    Public lblscore(2) As Label

    Public Sub Tbutton()

        'on connect les buttons et les labels qu'on défini au début avec les buttons et labels dans l'interface. 
        button(1) = Button1
        button(2) = Button2
        button(3) = Button3
        button(4) = Button4
        button(5) = Button5
        button(6) = Button6
        button(7) = Button7
        button(8) = Button8
        button(9) = Button9
        button(10) = Button10
        button(11) = Button11
        button(12) = Button12

        lblscore(1) = score1
        lblscore(2) = score2

    End Sub
    Public Sub InitialiseJeu()

        'On utilise les nom de utilisateurs pour les labels d'interface.

        nomJoueur1s.Text = nomJoueur1.Text
        nomJoueur2s.Text = nomjoueur2.Text
        lblscoreJ1.Text = "Score de " & nomJoueur1.Text & ":"
        lblscoreJ2.Text = "Score de " & nomjoueur2.Text & ":"

        'Au début, dans chaque trous, il y a 4 graines.
        For i = 1 To 12
            table(i) = 4
            button(i).Text = table(i)
        Next i

        'Score de chaque joueur est de 0.
        lblscore(1).Text = 0
        lblscore(2).Text = 0
        score(1) = 0
        score(2) = 0

        'on suppose que le joueur1 a la main premièrement et on met la flèche avant le joueur1. 
        joueur = 1
        flechejoueur1.Text = ">>>>>"
        flechejoueur2.Text = ""

    End Sub
    Public Sub btninitialise()

        'Par défaut, on suppose que le joueur 1 a la main, et tous les buttons attachés au joueur 1 sont disponible, et les autre sont indisponible.
        For i = 1 To 6
            button(i).Enabled = True
        Next
        For i = 7 To 12
            button(i).Enabled = False
        Next

    End Sub
    Public Sub bloque()

        'Dans cette procédure, on bloque tous les buttons.
        For i = 1 To 12
            button(i).Enabled = False
        Next

    End Sub

    Private Sub kakuha_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        'Au début le jeu, on utilise les procédures pour initialiser.  
        'Au debut, on demande les utilisateurs de entrer leur noms, si ils ne veulent pas les entrer, 
        'Les noms de utilisateurs sont 'Joueur 1' et 'Joueur 2' par défaut.

        nomJoueur1.Text = InputBox("Nom du joueur 1:（si vous ne voulez pas personaliser le nom, juste cliquez 'OK' directement. ）", "Entrez un nom de joueur")
        If nomJoueur1.Text = "" Then
            nomJoueur1.Text = "Joueur 1"
        End If
        nomjoueur2.Text = InputBox("Nom du joueur 2:（si vous ne voulez pas personaliser le nom, juste cliquez 'OK' directement. ）", "Entrez un nom de joueur")
        If nomjoueur2.Text = "" Then
            nomjoueur2.Text = "Joueur 2"
        End If

        Call Tbutton()
        Call InitialiseJeu()
        Call btninitialise()

    End Sub

    Private Sub Button_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click, Button2.Click, Button3.Click,
        Button4.Click, Button5.Click, Button6.Click, Button7.Click, Button8.Click, Button9.Click, Button10.Click, Button11.Click, Button12.Click

        'On défini 'i' comme le septième caractère du nom des buttons. 
        'Par exemple, les six premiers caractères de 'Button' est 'b''u''t''t''o''n', et le septième est de '1', et on converti le caractère '1' à byte '1'.
        Dim i As Integer = CByte(Mid(sender.name, 7))
        Dim g As Integer = CInt(button(i).Text) 'On défini 'g' comme le nombre des graines dans le trou.

        Jouer(i) 'Dans un premier pas, on utilise le fonction 'Jouer' pour calculer les évolution de nombre de sème dans les trous. 

        For j = 1 To 12 'On met à jour les text de button après le fonction 'Jouer'
            button(j).Text = table(j)
        Next

        lblscore(joueur).Text = CByte(lblscore(joueur).Text) + 1 'On ajoute score de 1 au joueur qui a la main.

        Call bloque() 'On bloque tous les buttons.

        If PeutJouer(i) = True Then 'Si le Boolean 'Peutjouer' est vrai, c'est-à-dire le trou suivant n'est pas vide. On peut débloquer le button qu'on doit choisir l'étape suivant.


            If i + g - 1 < 12 Then 'Condition 1: La somme du numéro de trou et le nombre de graines dans ce trou est inférieur à 12 
                If button(i + g).Text <> 0 Then
                    button(i + g).Enabled = True
                End If

            ElseIf i + g - 1 <= 24 Then 'Condition 2: Si i + g - 1 > = 12, le coup de jouer va retourner au premier trou.
                If button(i + g - 12).Text <> 0 Then
                    button(i + g - 12).Enabled = True
                End If

            Else
                If button(i + g - 24).Text <> 0 Then 'Condition 3: Si i + g - 1 > 24, le coup de jouer va faire 2 retour.
                    button(i + g - 24).Enabled = True
                End If
            End If

        Else 'si peutjouer est false, il faut changer le joueur

            MsgBox("C'est le temp de changer le joueur!", , "Changer joueur!")

            If joueur = 1 Then 'On initialise de la disponibilité de buttons après changer le joueur.
                For i = 1 To 6
                    If button(i).Text <> 0 Then
                        button(i).Enabled = True
                    End If
                Next
                flechejoueur1.Text = ">>>>>" 'quand le joueur a la main, il y a une flèche avant la label de nom de joueur.
                flechejoueur2.Text = ""
            Else
                For i = 7 To 12
                    If button(i).Text <> 0 Then
                        button(i).Enabled = True
                    End If
                Next
                flechejoueur1.Text = ""
                flechejoueur2.Text = ">>>>>"

            End If

        End If


        If JeuTermine(1) Then 'l'affichage de terminer de jeu 
            If score1.Text = score2.Text Then
                MsgBox(" Match nul ! ", , "Halalalala!")
                MsgBox(" Cliquez le button 'Rejouer' pour jouer un autre tour !!! Cliquez le button 'changer nom' pour changer le joueur !!! ", , "Vous voulez...")

            Else
                MsgBox(nomJoueur1.Text & " a gagné !", , "Halalalala!")
                MsgBox(" Cliquez le button 'Rejouer' pour jouer un autre tour !!! Cliquez le button 'changer nom' pour changer le joueur !!! ", , "Vous voulez...")
            End If

            scorefinaljoueur1.Items.Add(score(1) & "*") 'On ajoute le score dans la listbox de histoire, et on met '*' avant le score de joueur qui a gagné.
            scorefinaljoueur2.Items.Add(score(2))
            Call bloque()

        ElseIf JeuTermine(2) = True Then

            If score1.Text = score2.Text Then
                MsgBox("Match nul!", , "Halalalala!")
                MsgBox(" Cliquez le button 'Rejouer' pour jouer un autre tour !!! Cliquez le button 'changer nom' pour changer le joueur !!! ", , "Vous voulez...")
            Else
                MsgBox(nomjoueur2.Text & " a gagné! ", , "Halalalala!")
                MsgBox(" Cliquez le button 'Rejouer' pour jouer un autre tour !!! Cliquez le button 'changer nom' pour changer le joueur !!! ", , "Vous voulez...")
            End If

            scorefinaljoueur1.Items.Add(score(1))
            scorefinaljoueur2.Items.Add(score(2) & "*")
            Call bloque()
        End If


    End Sub

    Private Sub BtnRejouer_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnRejouer.Click

        'Quand on clique le button de Rejouer, si le joueur clique'OK', alors on initialise la disponibilté des buttons et les valeur des paramètres.
        'C'est pour éviter la situation que le joueur clique 'Rejoueur' accidentiellement.
        Dim r As Byte
        r = MsgBox("Voulez vous vraiment rejouer ?", 1, "Rejouer?")
        If r = vbOK Then
            Call InitialiseJeu()
            Call btninitialise()
        End If
        If r = vbCancel Then
        End If

    End Sub

    Private Sub Effacer_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Effacer.Click
        'quand on clique le button Effacer, les nombre de listbox de histoire sera effacés.
        scorefinaljoueur1.Items.Clear()
        scorefinaljoueur2.Items.Clear()

    End Sub

    Private Sub btnchangernom_Click(sender As System.Object, e As System.EventArgs) Handles btnchangernom.Click
        'Au debut, on demande les utilisateurs de entrer leur noms, si ils ne veulent pas les entrer, 
        'Les noms de utilisateurs sont 'Joueur 1' et 'Joueur 2' par défaut.
        nomJoueur1.Text = InputBox("Nom du joueur 1:", "Entrez un nom de joueur")
        If nomJoueur1.Text = "" Then
            nomJoueur1.Text = "Joueur 1"
        End If
        nomjoueur2.Text = InputBox("Nom du joueur 2:", "Entrez un nom de joueur")
        If nomjoueur2.Text = "" Then
            nomjoueur2.Text = "Joueur 2"
        End If
        Call InitialiseJeu()
        Call btninitialise()
    End Sub
End Class
