Public Class Form1Graph

    ' PUBLIC VARIABLES
    Dim ga_menu1 As New GA_MENU
    Dim sa_menu1 As New SA_MENU
    Dim MaxStr As Integer = 50
    Dim Locked As Boolean
    Dim StopNow As Boolean
    Dim pts(0) As Point
    Dim dist(0, 0) As Double
    Dim MaxIter As Int64
    Dim BMP_Points As Bitmap
    Dim BMP_Graph As Bitmap
    Dim GFX_Points As Graphics
    Dim GFX_Graph As Graphics
    Dim MaxX As Int32
    Dim MaxY As Int32
    Dim par As genetic_parameters
    Dim par1 As annealing_parameters
    ' Roulette fitness
    Dim rf(0) As Double

    Private Sub BACK_2_GENETIC_DoWork(sender As Object, e As System.ComponentModel.DoWorkEventArgs) Handles BACK_2_GENETIC.DoWork

        Dim iter_curr As Int64 = 1
        Dim iter_best As Int64 = 0
        Dim iter_last As Int64 = 0
        Dim val_curr As Double = 0
        Dim val_best As Double = 999999999
        Dim n As Integer = pts.Length - 1
        ' FORMING INITIAL TOUR
        Dim tour1 As New Tour(n)
        '-----------------------------------------------------------------------
        ' GENETIC ALGORITHM : METHOD 2

        ' FULL POPULATION
        Dim pop_size As Integer = par.population_size
        Dim population(pop_size) As Tour
        For i = 1 To pop_size
            population(i) = New Tour(n)
        Next i
        ' SELECTION INDEX
        Dim selection_index(pop_size, 1) As Integer
        ' INITIAL POPULATION
        Dim dice As Double
        Randomize()
        For i = 1 To pop_size
            dice = Rnd()
            If dice > 0.6 Then
                population(i).RandomNearestNeighbour(dist)
            Else
                population(i).Random(dist)
            End If
        Next i

        QuickSortTours(population, 1, pop_size)

        Dim diver As Double

        Do While Not StopNow And ToContinue(iter_curr)

            SELECTION(population, selection_index)
            CROSSOVER(population, selection_index)
            MUTATION(population)
            QuickSortTours(population, 1, pop_size)

            DrawTour(population(1))

            'diver = 0
            LabelLatestTour.Text = "Karta : " & iter_curr.ToString & "   Ilgis : " & Math.Round(population(1).dist, 1) & "   Turas : " & population(1).ToStringX(MaxStr)

            If iter_curr - iter_last > 250 Then
                iter_last = iter_curr
                diver = diversity(population)
                LabelBestTour.Text = "Diversifikacija : " & diver.ToString
            End If

            iter_curr += 1
        Loop
        ''-----------------------------------------------------------------------

        Call btnCmdStop_Click(sender, e)
    End Sub

    Private Sub BACK_3_ANNEALING_DoWork(sender As Object, e As System.ComponentModel.DoWorkEventArgs) Handles BACK_3_ANNEALING.DoWork

        Dim n As Integer = pts.Length - 1

        Dim iter_curr As Int64 = 1
        Dim tour1 As New Tour(n)
        Dim trial_tour As New Tour(n)
        Dim best_tour As New Tour(n)
        tour1.RandomNearestNeighbour(dist)
        best_tour.Copy(tour1)
        Dim temp As Double = par1.initial_temperature
        Dim delta As Double
        Dim swap_pt As Point
        Dim iter As Integer = 1
        Dim lasttemp As Double = par1.initial_temperature

        If par1.IsHomogenic Then

            ' HOMOGENOUS

            Do While temp > 0.02 And Not StopNow And ToContinue(iter_curr)

                For i = 1 To CInt(Math.Ceiling(n * par1.perturbations)) + 1
                    trial_tour.Copy(tour1)
                    swap_pt = rand_swap_pt(n)

                    trial_tour.Swap2Edges(swap_pt)
                    trial_tour.UpdateDist(dist)

                    delta = trial_tour.dist - tour1.dist
                    If delta < 0 Then
                        tour1.Copy(trial_tour)
                    Else
                        If Rnd() < Math.Exp(-(delta) / temp) Then
                            tour1.Copy(trial_tour)
                        End If
                    End If

                    If tour1.dist < best_tour.dist Then
                        best_tour.Copy(tour1)
                    End If

                    iter_curr += 1
                Next i


                DrawTour(tour1)
                LabelLatestTour.Text = "Temperatūra :   " & Math.Round(temp, 2).ToString("#0.00").PadRight(6) & "   Ilgis : " & Math.Round(tour1.dist, 1).ToString("#0.0").PadLeft(8, " ") & "   Turas : " & tour1.ToStringX(MaxStr)
                LabelBestTour.Text = "Geriausias rezultatas - " & " Ilgis : " & Math.Round(best_tour.dist, 1).ToString("#0.0").PadLeft(8, " ") & "   Turas : " & best_tour.ToStringX(MaxStr)
                temp *= par1.cooling_rate_alpha

                ' REANNEALING
                If temp < 0.02 Then
                    temp = par1.reannealing * lasttemp
                    lasttemp *= par1.reannealing
                End If
            Loop
        Else
            ' INHOMOGENOUS
            Do While temp > 0.02 And Not StopNow

                trial_tour.Copy(tour1)
                swap_pt = rand_swap_pt(n)
                trial_tour.Swap2Edges(swap_pt)
                trial_tour.UpdateDist(dist)

                delta = trial_tour.dist - tour1.dist

                If delta < 0 Then
                    tour1.Copy(trial_tour)
                Else
                    If Rnd() < Math.Exp(-(delta) / temp) Then
                        tour1.Copy(trial_tour)
                    End If
                End If

                If tour1.dist < best_tour.dist Then
                    best_tour.Copy(tour1)
                End If

                DrawTour(tour1)

                LabelLatestTour.Text = "Temperatūra :   " & Math.Round(temp, 2).ToString("#0.00").PadRight(6) & "   Ilgis : " & Math.Round(tour1.dist, 1).ToString("#0.0").PadLeft(8, " ") & "   Turas : " & tour1.ToStringX(MaxStr)
                LabelBestTour.Text = "Geriausias rezultatas - " & " Ilgis : " & Math.Round(best_tour.dist, 1).ToString("#0.0").PadLeft(8, " ") & "   Turas : " & best_tour.ToStringX(MaxStr)

                temp /= (1 + temp * par1.cooling_rate_beta)

                ' REANNEALING
                If temp < 0.02 Then
                    temp = par1.reannealing * lasttemp
                    lasttemp *= par1.reannealing
               End If
                iter_curr += 1
            Loop
        End If
        Call btnCmdStop_Click(sender, e)
    End Sub

    ' COMMON FUNCTIONS
    Function FormDistanceMatrix()
        Dim n As Integer = pts.Length - 1

        ReDim dist(n, n)

        For i = 1 To n
            For j = 1 To n
                If Not i.Equals(j) Then
                    dist(i, j) = Math.Sqrt((pts(i).X - pts(j).X) * (pts(i).X - pts(j).X) + (pts(i).Y - pts(j).Y) * (pts(i).Y - pts(j).Y))
                Else
                    dist(i, j) = 0
                End If
            Next j
        Next i

        Return Nothing
    End Function

    Function diversity(ByVal pop As Tour()) As Double
        Dim div As Double = 0
        Dim pd As Integer = pop.Length - 1
        Dim n As Integer = pop(1).Length - 1

        For i = 1 To pd - 1
            For j = i + 1 To pd
                div += hamming_dist(pop(i), pop(j))
            Next
        Next i
        div = (2 * div) / (pd * (pd - 1) * n)

        Return div
    End Function

    Function hamming_dist(ByVal tour1 As Tour, ByVal tour2 As Tour) As Integer
        Dim n As Integer = tour1.Length - 1
        Dim match As Integer = 0

        Dim edges1(n, 1) As Integer

        edges1(tour1.city(1), 0) = tour1.city(n)
        edges1(tour1.city(1), 1) = tour1.city(2)

        For i = 2 To n - 1
            edges1(tour1.city(i), 0) = tour1.city(i - 1)
            edges1(tour1.city(i), 1) = tour1.city(i + 1)
        Next i
        edges1(tour1.city(n), 0) = tour1.city(n - 1)
        edges1(tour1.city(n), 1) = tour1.city(1)

        For i = 1 To n - 1
            If edges1(tour2.city(i), 0) = tour2.city(i + 1) Or edges1(tour2.city(i), 1) = tour2.city(i + 1) Then
                match += 1
            End If
        Next i
        If edges1(tour2.city(n), 0) = tour2.city(1) Or edges1(tour2.city(n), 1) = tour2.city(1) Then
            match += 1
        End If

        Return n - match
    End Function

    ' FULL SEARCH FUNCTIONS
    Function RestorePemutation(ByVal subset As Integer(), ByVal first As Integer, ByVal last As Integer) As Tour

        Dim n As Integer = subset.Length + 3
        Dim tour1 As New Tour(n)

        tour1.city(1) = 1
        tour1.city(2) = first
        tour1.city(n) = last

        For o = 3 To n - 1
            tour1.city(o) = subset(o - 3)
        Next o

        Return tour1
    End Function

    ' GENETIC FUNCTIONS


    ' GENETIC FUNCTIONS : SELECTION

    Sub SELECTION(ByVal pop As Tour(), ByRef sel As Integer(,))
        Dim n As Integer = pop.Length - 1

        ' RESET SELECTION INDEX
        For i = 0 To pop.Length - 1
            sel(i, 0) = 0
            sel(i, 1) = 0
        Next i

        ' CREATE APPROPRIATE FITNESSES
        Select Case par.selection
            Case "p_roulette"
                _PROPORTIONAL_ROULETTE_FITNESS(pop)
            Case "tournament"
                _INVERSE_FITNESS(pop)
            Case "stochastic"

            Case "l_rank_roulette"
                _LINEAR_RANK_FITNESS(pop)
            Case "inv_rank"
                _INVERSE_RANK_FITNESS(pop)
        End Select



        ' RANDOM PARENTS DRAW
        Dim parents As Point
        For i = par.elites + 1 To n


            Select Case par.selection
                Case "p_roulette"
                    parents = _PROPORTIONAL_ROULETTE()
                Case "tournament"
                    parents = _TOURNAMENT(par.tournament_size)
                Case "stochastic"
                    parents = _TOURNAMENT(1)
                Case "l_rank_roulette"
                    parents = _LINEAR_RANK()
                Case "inv_rank"
                    parents = _INVERSE_RANK()
            End Select

            sel(i, 0) = parents.X
            sel(i, 1) = parents.Y
        Next i



    End Sub

    Function _TOURNAMENT(ByVal tsize As Integer) As Point
        Dim pop_size As Integer = par.population_size
        Dim pt1 As Point

        Dim indx1, indx2 As Double
        Dim best As Integer = 0

        best = rand(1, pop_size)
        For i = 2 To tsize
            indx1 = rand(1, pop_size)
            If rf(indx1) > rf(best) Then
                best = indx1
            End If
        Next
        pt1.X = best


        pt1.Y = pt1.X
        Do While pt1.X = pt1.Y
            best = rand(1, pop_size)
            For i = 2 To tsize
                indx2 = rand(1, pop_size)
                If rf(indx2) > rf(best) Then
                    best = indx2
                End If
            Next
            pt1.Y = best
        Loop

        Return pt1
    End Function
    Sub _INVERSE_FITNESS(ByVal pop As Tour())
        Dim pop_size As Integer = pop.Length - 1

        ReDim rf(pop_size)

        For i = 1 To pop_size
            rf(i) = 1 / pop(i).dist
        Next i

    End Sub

    Function _PROPORTIONAL_ROULETTE() As Point
        Dim pop_size As Integer = par.population_size
        Dim pt1 As Point

        Dim val1, val2 As Double
        Dim indx As Integer = 1

        Randomize()
        val1 = Rnd()
        Do While val1 > rf(indx)
            indx += 1
        Loop
        pt1.X = indx

        pt1.Y = indx
        Do While pt1.Y = pt1.X
            indx = 1
            val2 = Rnd()
            Do While val2 > rf(indx)
                indx += 1
            Loop
            pt1.Y = indx
        Loop

        Return pt1
    End Function
    Sub _PROPORTIONAL_ROULETTE_FITNESS(ByVal pop As Tour())
        Dim pop_size As Integer = pop.Length - 1

        ReDim rf(pop_size)

        For i = 1 To pop_size
            rf(i) = pop(i).dist
        Next i
        rf(0) = Double.MaxValue
        Dim fmin As Double = rf.Min()
        rf(0) = 0

        For i = 1 To pop_size
            rf(i) = fmin / rf(i)
        Next i

        For i = 1 To pop_size
            rf(i) += rf(i - 1)
        Next i

        For i = 1 To pop_size
            rf(i) /= rf(pop_size)
        Next i

    End Sub

    Function _LINEAR_RANK() As Point
        Return _PROPORTIONAL_ROULETTE()
    End Function
    Sub _LINEAR_RANK_FITNESS(ByVal pop As Tour())
        Dim pop_size As Integer = pop.Length - 1
        Dim s As Double = par.selective_pressure

        ReDim rf(pop_size)

        For i = 1 To pop_size
            rf(i) = 2 - s + 2 * (s - 1) * ((pop_size - i) / (pop_size - 1))
        Next i

        For i = 1 To pop_size
            rf(i) += rf(i - 1)
        Next i

        For i = 1 To pop_size
            rf(i) /= rf(pop_size)
        Next i
    End Sub

    Function _INVERSE_RANK() As Point
        Return _PROPORTIONAL_ROULETTE()
    End Function
    Sub _INVERSE_RANK_FITNESS(ByVal pop As Tour())
        Dim pop_size As Integer = pop.Length - 1
        Dim s As Double = par.selective_pressure

        ReDim rf(pop_size)
        For i = 1 To pop_size
            rf(i) = 1 / i
        Next i

        For i = 1 To pop_size
            rf(i) += rf(i - 1)
        Next i

        For i = 1 To pop_size
            rf(i) /= rf(pop_size)
        Next i

    End Sub

    ' GENETIC FUNCTIONS : CROSSOVER

    Sub CROSSOVER(ByRef pop As Tour(), ByVal sel As Integer(,))
        Dim n As Integer = pop.Length - 1
        Dim tour_length As Integer = pop(1).Length - 1
        Dim val1 As Double
        Dim temp(0) As Tour

        ' RESET CHILDREN INDEX IN POPULATION
        For j = 1 To n
            pop(j).city(0) = 0
        Next j

        ' PERFORM RANDOM CROSSOVER AND MARK CROSSOVER ID
        Dim crossCount As Integer = 0
        For i = par.elites + 1 To n
            val1 = Rnd()
            If val1 < par.prob_crossover Then
                crossCount += 1
                ReDim Preserve temp(crossCount)
                temp(crossCount) = New Tour(tour_length)

                temp(crossCount).Copy(CROSSOVER_OPERATOR(pop(sel(i, 0)), pop(sel(i, 1))))

                pop(i).city(0) = -1
            End If
        Next

        ' COPY CHILDREN TO THEIR PLACES IN POPULATION
        Dim k As Integer = 1
        For j = 1 To n
            If pop(j).city(0) = -1 Then
                pop(j).Copy(temp(k))
                pop(j).city(0) = -1
                k += 1
            End If
        Next j

    End Sub

    Function CROSSOVER_OPERATOR(ByVal parent1 As Tour, ByVal parent2 As Tour) As Tour

        Select Case par.crossover
            Case "ULX"
                Return ULX(parent1, parent2)
            Case "PMX"
                Return PMX(parent1, parent2)
            Case "OX1"
                Return OX1(parent1, parent2)
            Case "CX"
                Return CX(parent1, parent2)
            Case "SPX"
                Return SPX(parent1, parent2)
            Case "EX"
                Return EX(parent1, parent2)
        End Select

        Return parent1
    End Function

    Function ULX(ByVal parent1 As Tour, ByVal parent2 As Tour) As Tour
        Dim n As Integer = parent1.Length - 1
        Dim tour1 As New Tour(n)
        Dim bits(n) As Boolean

        Dim elem(n) As Integer
        For i = 1 To n
            elem(i) = i
        Next i

        Dim idx As Integer
        For i = 1 To n
            If Not bits(parent1.city(i)) And Not bits(parent2.city(i)) Then
                If Rnd() > 0.5 Then
                    tour1.city(i) = parent1.city(i)
                    bits(parent1.city(i)) = 1
                    CutElement(elem, parent1.city(i))
                Else
                    tour1.city(i) = parent2.city(i)
                    bits(parent2.city(i)) = 1
                    CutElement(elem, parent2.city(i))
                End If
            ElseIf bits(parent1.city(i)) And bits(parent2.city(i)) Then
                idx = rand(1, elem.Length - 1)
                tour1.city(i) = elem(idx)
                bits(elem(idx)) = 1
                CutElement(elem, elem(idx))
            ElseIf Not bits(parent1.city(i)) And bits(parent2.city(i)) Then
                tour1.city(i) = parent1.city(i)
                bits(parent1.city(i)) = 1
                CutElement(elem, parent1.city(i))
            ElseIf bits(parent1.city(i)) And Not bits(parent2.city(i)) Then
                tour1.city(i) = parent2.city(i)
                bits(parent2.city(i)) = 1
                CutElement(elem, parent2.city(i))
            End If
        Next i

        Return tour1
    End Function

    Function PMX(ByVal parent1 As Tour, ByVal parent2 As Tour) As Tour
        Dim n As Integer = parent1.Length - 1
        Dim tour1 As New Tour(n)
        tour1.Nullify()
        Dim mask(n) As Integer

        Randomize()
        Dim cut_length As Integer = CInt(Int(((n - 2) * Rnd()) + 1))
        Dim cut_start As Integer = CInt(Int(((n - cut_length + 1) * Rnd()) + 1))
        Dim cut_end As Integer = cut_start + cut_length - 1

        tour1.PutSegment(cut_start, parent1, cut_start, cut_length)


        Dim dupes(0) As Integer
        Dim k As Integer = 0

        Dim idx As Integer
        For i = cut_start To cut_end

            idx = parent2.FindIndexOf(parent1.city(i), 1, n)

            If idx >= cut_start And idx <= cut_end Then
                If Not i = idx Then
                    k = dupes.Length
                    ReDim Preserve dupes(k)
                    dupes(k) = i
                End If
            Else
                tour1.city(idx) = parent2.city(i)
            End If

        Next i


        For i = 1 To cut_start - 1
            If tour1.city(i) = 0 Then
                tour1.city(i) = parent2.city(i)
            End If
        Next i
        For i = cut_start + cut_length To n
            If tour1.city(i) = 0 Then
                tour1.city(i) = parent2.city(i)
            End If
        Next i

        Dim duplicate As Integer
        Dim replacer As Integer
        For i = 1 To dupes.Length - 1
            duplicate = parent1.city(dupes(i))
            replacer = parent2.city(dupes(i))

            idx = tour1.FindIndexOf(duplicate, 1, n)
            'idx = tour1.FindIndexOf(duplicate, 1, cut_start - 1)
            'If idx = -1 Then
            '    idx = tour1.FindIndexOf(duplicate, cut_end + 1, n)
            'End If

            tour1.city(idx) = replacer
        Next i


        Return tour1
    End Function

    Function OX1(ByVal parent1 As Tour, ByVal parent2 As Tour) As Tour
        Dim n As Integer = parent1.Length - 1
        Dim tour1 As New Tour(n)

        Dim idx As Point = rand2(1, n)

        Dim mask(n) As Integer

        For i = idx.X To idx.Y
            tour1.city(i) = parent1.city(i)
            mask(parent1.city(i)) = 1
        Next i


        Dim k As Integer = 1
        For i = 1 To idx.X - 1
            Do While mask(parent2.city(k)) = 1
                k += 1
            Loop
            tour1.city(i) = parent2.city(k)
            k += 1
        Next i

        For i = idx.Y + 1 To n
            Do While mask(parent2.city(k)) = 1
                k += 1
            Loop
            tour1.city(i) = parent2.city(k)
            k += 1
        Next i

        Return tour1
    End Function

    Function CX(ByVal parent1 As Tour, ByVal parent2 As Tour) As Tour
        Dim n As Integer = parent1.Length - 1
        Dim tour1 As New Tour(n)
        Dim mask(1, n) As Boolean

        ' row 0 - is cycle element ? row 1 - cycle of parent1 ?

        Dim mark As Boolean = True

        Dim count, pos As Integer
        count = 0

        pos = 1
        Do While count < n

            count += CX_MARK_CYCLE(parent1, parent2, pos, mask, mark)
            mark = (mark = False)
            pos = FindIndexOfFirstBoolean(mask, 0, 0)
        Loop

        For i = 1 To n
            If mask(1, i) Then
                tour1.city(i) = parent1.city(i)
            Else
                tour1.city(i) = parent2.city(i)
            End If
        Next i


        Return tour1
    End Function

    Function CX_MARK_CYCLE(ByVal parent1 As Tour, ByVal parent2 As Tour, ByVal start_idx As Integer, ByRef mask As Boolean(,), ByVal marker As Boolean) As Integer
        Dim count As Integer
        Dim n As Integer = parent1.Length - 1

        Dim start1, pointer, idx As Integer

        start1 = parent1.city(start_idx)
        pointer = parent2.city(start_idx)

        mask(0, start_idx) = True
        mask(1, start_idx) = marker
        count = 1

        Do While Not pointer = start1
            idx = parent1.FindIndexOf(pointer, 1, n)
            mask(0, idx) = True
            mask(1, idx) = marker
            count += 1
            pointer = parent2.city(idx)
        Loop


        Return count
    End Function

    Function SPX(ByVal parent1 As Tour, ByVal parent2 As Tour) As Tour
        Dim n As Integer = parent1.Length - 1
        Dim mask(1, n) As Boolean

        Dim p1, p2, c1, c2, best As New Tour(n)
        Dim idx As Integer
        best.dist = Double.MaxValue

        p1.Copy(parent1)
        p2.Copy(parent2)

        For i = 1 To n
            c1.Copy(p1)
            c2.Copy(p2)

            c1.city(i) = p2.city(i)
            idx = p1.FindIndexOf(p2.city(i), 1, n)
            c1.city(idx) = p1.city(i)

            c2.city(i) = p1.city(i)
            idx = p2.FindIndexOf(p1.city(i), 1, n)
            c2.city(idx) = p2.city(i)

            c1.UpdateDist(dist)
            c2.UpdateDist(dist)

            If c1.dist < c2.dist Then
                p1.Copy(c1)
                If c1.dist < best.dist Then
                    best.Copy(p1)
                End If
            Else
                p2.Copy(c2)
                If c2.dist < best.dist Then
                    best.Copy(p2)
                End If
            End If
        Next i

        Return best
    End Function
    ' GENETIC FUNCTIONS : MUTATION
    Sub MUTATION(ByRef pop As Tour())
        Dim n As Integer = pop.Length - 1
        Dim cityCount As Integer = pop(1).Length - 1
        Dim val As Double

        ' UPDATE ALL CHILDREN DISTANCES, MUTATE IF PROB
        For i = 1 To n
            If pop(i).city(0) = -1 Then
                val = Rnd()
                If val < par.prob_mutation Then
                    MUTATION_OPERATOR(pop(i))
                End If
                pop(i).UpdateDist(dist)
            End If
        Next
    End Sub

    Sub MUTATION_OPERATOR(ByRef tourx As Tour)
        If par.mutation = "SWAP2E" Then
            SWAP2E(tourx)
        ElseIf par.mutation = "SWAP2C" Then
            SWAP2C(tourx)
        ElseIf par.mutation = "INSERT1C" Then
            INSERT1C(tourx)
        ElseIf par.mutation = "PSM" Then
            PSM(tourx)
        ElseIf par.mutation = "RND_K_SWAP" Then
            RND_K_SWAP(tourx)
        End If
    End Sub

    Sub SWAP2E(ByRef tourx As Tour) ' INVERSION
        Dim cityCount As Integer = tourx.Length - 1
        For i = 1 To 2
            Dim swap_pt As Point = rand_swap_pt(cityCount)
            tourx.Swap2Edges(swap_pt)
        Next i
    End Sub

    Sub SWAP2C(ByRef tourx As Tour) ' EXCHANGE
        Dim cityCount As Integer = tourx.Length - 1
        Dim pt1 As Point = rand_swap_pt(cityCount)
        tourx.SwapCitiesAt(pt1.X, pt1.Y)
    End Sub

    Sub RND_K_SWAP(ByRef tourx As Tour) ' <=K EDGES SWAP
        Dim cityCount As Integer = tourx.Length - 1
        Dim tour1 As New Tour(cityCount)

        Dim k As Integer = rand(2, par.max_swap)

        Dim points(k) As Integer
        points = randK(1, cityCount, k)
        ' PLACING END TAIL TO FRONT SEGMENT
        Dim max As Integer = points.Max
        For i = max + 1 To cityCount
            tour1.city(i - max) = tourx.city(i)
        Next i
        Dim shift As Integer = cityCount - max
        For i = 1 To max
            tour1.city(shift + i) = tourx.city(i)
        Next i

        For i = 1 To k
            points(i) += shift
        Next i

        Dim taken(cityCount) As Boolean

        taken(0) = 1
        For i = 1 To k
            taken(points(i)) = True
        Next i


        Dim inv As Point
        Dim j, idx, pos As Integer
        pos = cityCount
        For i = 1 To k
            idx = points(i)
            tourx.city(pos) = tour1.city(idx)
            pos -= 1

            j = 1
            Do While Not taken(idx - j)
                tourx.city(pos) = tour1.city(idx - j)
                pos -= 1
                j += 1
            Loop

            If Rnd() < 0.5 Then
                inv.X = pos + 1
                inv.Y = pos + j
                tourx.InvertSegment(inv)
            End If
        Next i

    End Sub
    Sub INSERT1C(ByRef tourx As Tour) ' INSERTION
        Dim cityCount As Integer = tourx.Length - 1
        Dim pt1 As Point = rand_swap_pt(cityCount)

        Dim val As Double = Rnd()
        If val > 0.5 Then
            InvertPoint(pt1)
        End If

        Dim tour1 As New Tour(cityCount)
        tour1.Copy(tourx)
        If pt1.X < pt1.Y Then
            For i = pt1.X To pt1.Y - 1
                tour1.city(i) = tourx.city(i + 1)
            Next i
            tour1.city(pt1.Y) = tourx.city(pt1.X)
        Else
            tour1.city(pt1.Y + 1) = tourx.city(pt1.X)
            For i = pt1.Y + 2 To pt1.X
                tour1.city(i) = tourx.city(i - 1)
            Next i
        End If
        tourx.Copy(tour1)
    End Sub

    Sub PSM(ByRef tourx As Tour) ' PARTIAL SHUFFLE MUTATION
        Dim cityCount As Integer = tourx.Length - 1

        Dim idx As Integer
        Randomize()
        For i = 1 To cityCount
            If Rnd() < par.prob_psm Then
                idx = rand(1, cityCount)
                tourx.SwapCitiesAt(i, idx)
            End If
        Next i
    End Sub

End Class