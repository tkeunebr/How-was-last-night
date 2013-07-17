;;;
;;; TP3
;;; Bonne/mauvaise soir�e ? 
;;;

;;reload
(defun rl ()
	(load "IA01_tp3.lisp")
)

;;fonction qui lance le syst�me expert
(defun start ()
	(rl)
	(moteur_inf *br* *bf* nil)
)

;;;cr�ation de la base des faits
(setq *BF* 
	'(
		;musique et dance
		(presence_musique (oui non NR) "Est-ce qu'il y avait de la musique ?" (niveau 1))
		(correspondance_gout_musique (oui non NR) "Est-ce que la musique correspondait � votre attente?" (niveau 2))
		(volume_sonore (trop_fort parfait trop_faible NR) "Comment avez-vous trouv� le volume sonore ?" (niveau 2))
		(qualite_son (bonne neutre mauvaise NR) "D�duction � faire" (niveau 3))
		(utilisateur_dance (toute_la_soiree parfois jamais NR) "Vous avez dans� ..." (niveau 2))
		(autre_dance (personne des_garcons des_filles tout_le_monde NR) "Parmi les invit�s, qui dansait ?" (niveau 2))
		(qualite_dancefloor (excellente bonne neutre mauvaise NR) "D�duction � faire" (niveau 3))
		(bilan_musique (tres_positif positif neutre negatif tres_negatif NR) "D�duction � faire" (niveau 4))

		;alcool
		(alcool_presence (oui non NR) "Estce qu'il y avait de l'alcool � la soir�e ?" (niveau 1))
		(quantite_alcool_debut (beaucoup normale peu NR) "Quelle �tait la quantit� d'alcool en d�but de soir�e ?" (niveau 2))
		(quantite_alcool_fin (beaucoup normale peu rien NR) "Quelle �tait la quantit� d'alcool en fin de soir�e ?" (niveau 2))
		(etat_general_ebriete (trop_saoul joyeux trop_sobre NR) "D�duction � faire" (niveau 3))
		(vomi (oui non NR) "Avez-vous vomi pendant la soir�e ?" (niveau 2))
		(heure_vomi (debut_soiree milieu_soiree fin_soiree NR) "A quel moment de la soir�e avez-vous vomi ?" (niveau 3))
		(bilan_boisson (positif neutre negatif NR) "D�duction � faire" (niveau 4))

		;socialisation
		(rencontre (oui non NR) "Avez-vous rencontr� des gens durant la soir�e ?" (niveau 1))
		(type_rencontre (amicale flirt neutre decevante NR) "Comment qualifiriez-vous votre rencontre ?" (niveau 2))
		(new_gf_bf (non probablement_oui oui NR) "D�duction � faire" (niveau 4))
		(coucher_avec (une_fois plusieurs_fois non NR) "Avez-vous couch� avec cette personne?" (niveau 3))
		(bilan_rencontre (positif neutre negatif NR) "D�duction � faire" (niveau 5))
		(bagarre_soiree (baston_violente petite_embrouille non NR) "Y a-t-il eu des conlits ?" (niveau 1))
		(implication (inconnus amis vous NR) "Qui �tait impliqu� ?" (niveau 2))
		(bilan_violence (positif neutre negatif NR) "D�duction � faire" (niveau 5))
		(bilan_socialisation (positif neutre negatif NR) "D�duction � faire" (niveau 6))

		;divertissement
		(perdu_qqc (oui non NR) "Avez-vous perdu quelque chose pendant la soir�e ?" (niveau 1))
		(radin (oui un_peu non NR) "Etes-vous proche de votre argent ?" (niveau 2))
		(valeur_perte_sentiment (aucune petite grande enorme NR) "Quelle est la valeur sentimentale de la perte ?" (niveau 2))
		(valeur_perte_argent (aucune petite grande enorme NR) "Quelle est la valeur financi�re de la perte ?" (niveau 2))
		(valeur_perte (aucune petite grande enorme NR) "D�duction � faire" (niveau 3))
		(dance_du_limousin (plusieurs une aucune NR) "Combien de danses du limousin ont eu lieu ?" (niveau 1))
		(bilan_divertissement (positif neutre negatif) "D�duction � faire" (niveau 4))
		 
		 ;nourriture
		(nourriture_presence (oui non NR) "Est-ce qu'il y avait � manger pendant la soir�e ?" (niveau 1))
		(quantite_nourriture_debut (beaucoup normal peu NR) "Quelle �tait la quantit� de nourriture en d�but de soir�e ?" (niveau 2))
		(quantite_nourriture_fin (beaucoup normal peu rien NR) "Quelle �tait la quantit� de nourriture en fin de soir�e ?" (niveau 2))
		(rapport_quantite_succes (nourriture_pas_bonne trop_nourriture parfait trop_affame pas_assez_�_manger NR) "D�duction � faire" (niveau 3))
		(gout_personnel (tres_bonne correcte bofbof mauvaise NR) "Comment avez-vous trouv� la nourriture ?" (niveau 3))
		(bilan_nourriture (positif neutre negatif NR) "D�duction � faire" (niveau 4))

		;solution
		(solution (bonne_soiree soiree_mitigee mauvaise_soiree NR) "D�duction � faire" (niveau 10))
	)
)

;;;
;;;		DEBUT
;;;	fonctions de service
;;;       faits
;;;

;;;retourne la question d'un fait
(defun q_f (nom_fait base_faits)
	(caddr (assoc nom_fait base_faits))
)

;;retourne le niveau d'un fait
(defun lvl_f (nom_fait base_faits)
	(cadr (cadddr (assoc nom_fait base_faits)))
)

;;retourne les valeurs possibles pour un fait
(defun vp_f (nom_fait base_faits)
	(cadr (assoc nom_fait base_faits))
)


;;;initialisation de tous les faits avec la valeur NR
(defun initialisation_faits (base_faits)

	(dolist (fait base_faits T)
		(set (car fait) 'NR)
	)
)

;;;affichage de la base des faits � un instant donn�
(defun affichage_bf (base_faits)
	(format T "~%~%d�but de la base des faits ~%~%")
	(dolist (fait base_faits T)
		(format T "~A ~A ~%" (car fait) (eval (car fait)))
	)
	(format T "~%fin de la base des faits ~%~%")
)

;;;retourne le premier fait NR
(defun premier_fait_NR(base_faits)
	(let ((res NIL))
		(do ((b base_faits (cdr b))) ((or (equal b NIL) (not (equal res NIL)))  res)
			(if (equal (eval (caar b)) 'NR) (setq res (car b)))
		)
	)
)

;;;
;;;    faits
;;; fonctions de service
;;;     FIN


;;;cr�ation de la base des regles
(setq *BR* 
	'(
		;musique et dance
		(RM1 (equal presence_musique 'non) (and (setq bilan_musique 'negatif) (setq correspondance_gout_musique 'NN) (setq volume_sonore 'NN) (setq qualite_son 'NN) (setq utilisateur_dance 'NN) (setq autre_dance 'NN) (setq qualite_dancefloor 'NN)))
		(RM2 (equal presence_musique 'oui) (and (question_fait 'correspondance_gout_musique *bf*) (question_fait 'volume_sonore *bf*) (question_fait 'utilisateur_dance *bf*) (question_fait 'autre_dance *bf*)))
		(RM3 (and (equal correspondance_gout_musique 'non) (not (equal volume_sonore 'parfait))) (setq qualite_son 'mauvaise))
		(RM4 (and (equal correspondance_gout_musique 'oui) (not (equal volume_sonore 'parfait))) (setq qualite_son 'neutre))
		(RM5 (and (equal correspondance_gout_musique 'non) (equal volume_sonore 'parfait)) (setq qualite_son 'neutre))
		(RM6 (and (equal correspondance_gout_musique 'oui) (equal volume_sonore 'parfait)) (setq qualite_son 'bonne))
		(RM7 (and (equal utilisateur_dance 'jamais) (not (equal autre_dance 'tout_le_monde))) (setq qualite_dancefloor 'mauvaise))
		(RM8 (and (equal utilisateur_dance 'jamais) (equal autre_dance 'tout_le_monde)) (setq qualite_dancefloor 'neutre))
		(RM9 (and (equal utilisateur_dance 'parfois) (equal autre_dance 'personne)) (setq qualite_dancefloor 'mauvaise))
		(RM10 (and (equal utilisateur_dance 'parfois) (or (equal autre_dance 'des_garcons) (equal autre_dance 'des_filles))) (setq qualite_dancefloor 'neutre))
		(RM12 (and (equal utilisateur_dance 'parfois) (equal autre_dance 'tout_le_monde)) (setq qualite_dancefloor 'bonne))
		(RM13 (and (equal utilisateur_dance 'toute_la_soiree) (equal autre_dance 'personne)) (setq qualite_dancefloor 'neutre))
		(RM14 (and (equal utilisateur_dance 'toute_la_soiree) (or (equal autre_dance 'des_garcons) (equal autre_dance 'des_filles))) (setq qualite_dancefloor 'bonne))
		(RM15 (and (equal utilisateur_dance 'toute_la_soiree) (equal autre_dance 'tout_le_monde)) (setq qualite_dancefloor 'excellente))
		(RM16 (and (equal qualite_son 'mauvaise) (equal qualite_dancefloor 'mauvaise)) (setq bilan_musique 'tres_negatif))
		(RM17 (and (equal qualite_son 'mauvaise) (equal qualite_dancefloor 'neutre)) (setq bilan_musique 'negatif))
		(RM18 (and (equal qualite_son 'neutre) (equal qualite_dancefloor 'neutre)) (setq bilan_musique 'neutre))
		(RM19 (and (equal qualite_son 'bonne) (equal qualite_dancefloor 'mauvaise)) (setq bilan_musique 'negatif))
		(RM20 (and (equal qualite_son 'mauvaise) (equal qualite_dancefloor 'bonne)) (setq bilan_musique 'negatif))
		(RM21 (and (equal qualite_son 'bonne) (equal qualite_dancefloor 'neutre)) (setq bilan_musique 'positif))
		(RM22 (and (equal qualite_son 'neutre) (equal qualite_dancefloor 'bonne)) (setq bilan_musique 'positif))
		(RM23 (and (equal qualite_son 'bonne) (equal qualite_dancefloor 'bonne)) (setq bilan_musique 'positif))
		(RM24 (and (equal qualite_son 'bonne) (equal qualite_dancefloor 'excellente)) (setq bilan_musique 'tres_positif))
		(RM25 (and (equal qualite_son 'neutre) (equal qualite_dancefloor 'excellente)) (setq bilan_musique 'positif))
		(RM26 (and (equal qualite_son 'mauvaise) (equal qualite_dancefloor 'excellente)) (setq bilan_musique 'neutre))

		;alcool
		(RB1 (equal alcool_presence 'non) (and (setq bilan_boisson 'negatif) (setq quantite_alcool_debut 'NN) (setq quantite_alcool_fin 'NN) (setq etat_general_ebriete 'NN) (setq vomi 'NN) (setq heure_vomi 'NN) ))
		(RB2 (equal alcool_presence 'oui) (and (question_fait 'quantite_alcool_debut *bf*) (question_fait 'quantite_alcool_fin *bf*) (question_fait 'vomi *bf*)))
		(RB3 (equal vomi 'oui) (question_fait 'heure_vomi *bf*))
		(RB4 (and (equal quantite_alcool_debut 'beaucoup) (equal quantite_alcool_fin 'beaucoup)) (setq etat_general_ebriete 'trop_sobre))
		(RB5 (and (equal quantite_alcool_debut 'beaucoup) (equal quantite_alcool_fin 'normale)) (setq etat_general_ebriete 'joyeux))
		(RB6 (and (equal quantite_alcool_debut 'beaucoup) (equal quantite_alcool_fin 'peu)) (setq etat_general_ebriete 'joyeux))
		(RB7 (and (equal quantite_alcool_debut 'beaucoup) (equal quantite_alcool_fin 'rien)) (setq etat_general_ebriete 'trop_saoul))
		(RB8 (and (equal quantite_alcool_debut 'normale) (equal quantite_alcool_fin 'normale)) (setq etat_general_ebriete 'trop_sobre))
		(RB9 (and (equal quantite_alcool_debut 'normale) (equal quantite_alcool_fin 'peu)) (setq etat_general_ebriete 'joyeux))
		(RB10 (and (equal quantite_alcool_debut 'normale) (equal quantite_alcool_fin 'rien)) (setq etat_general_ebriete 'trop_saoul))
		(RB11 (and (equal quantite_alcool_debut 'peu) (equal quantite_alcool_fin 'peu)) (setq etat_general_ebriete 'trop_sobre))
		(RB12 (and (equal quantite_alcool_debut 'peu) (equal quantite_alcool_fin 'rien)) (setq etat_general_ebriete 'joyeux))
		(RB13 (and (equal vomi 'non) (equal etat_general_ebriete 'joyeux)) (setq bilan_boisson 'positif))
		(RB14 (and (equal vomi 'non) (equal etat_general_ebriete 'trop_saoul)) (setq bilan_boisson 'negatif))
		(RB15 (and (equal vomi 'non) (equal etat_general_ebriete 'trop_sobre)) (setq bilan_boisson 'negatif))
		(RB16 (and (equal vomi 'oui) (equal heure_vomi 'debut_soiree)) (setq bilan_boisson 'negatif))
		(RB17 (and (equal vomi 'oui) (equal heure_vomi 'milieu_soiree) (equal etat_general_ebriete 'joyeux)) (setq bilan_boisson 'neutre))
		(RB18 (and (equal vomi 'oui) (equal heure_vomi 'milieu_soiree) (equal etat_general_ebriete 'trop_saoul)) (setq bilan_boisson 'negatif))
		(RB19 (and (equal vomi 'oui) (equal heure_vomi 'milieu_soiree) (equal etat_general_ebriete 'trop_sobre)) (setq bilan_boisson 'negatif))
		(RB20 (and (equal vomi 'oui) (equal heure_vomi 'fin_soiree) (equal etat_general_ebriete 'joyeux)) (setq bilan_boisson 'positif))
		(RB21 (and (equal vomi 'oui) (equal heure_vomi 'fin_soiree) (equal etat_general_ebriete 'trop_saoul)) (setq bilan_boisson 'neutre))
		(RB22 (and (equal vomi 'oui) (equal heure_vomi 'fin_soiree) (equal etat_general_ebriete 'trop_sobre)) (setq bilan_boisson 'negatif))
		(RB23 (equal vomi 'non) (setq heure_vomi 'NN))

		;socialisation
		(RS1 (equal rencontre 'oui) (question_fait 'type_rencontre *BF*))
		(RS2 (equal type_rencontre 'flirt) (question_fait 'coucher_avec *BF*))
		(RS3 (equal type_rencontre 'amicale) (and (setq bilan_rencontre 'positif) (setq coucher_avec 'NN) (setq new_gf_bf 'NN)))
		(RS4 (equal type_rencontre 'neutre) (and (setq bilan_rencontre 'neutre) (setq coucher_avec 'NN) (setq new_gf_bf 'NN)))
		(RS5 (equal type_rencontre 'decevante) (and (setq bilan_rencontre 'negatif) (setq coucher_avec 'NN) (setq new_gf_bf 'NN)))
		(RS6 (equal coucher_avec 'une_fois) (setq new_gf_bf 'probablement_oui))
		(RS7 (equal coucher_avec 'plusieurs_fois) (setq new_gf_bf 'oui))
		(RS8 (equal coucher_avec 'non) (setq new_gf_bf 'non))
		(RS9 (equal new_gf_bf 'non) (setq bilan_rencontre 'negatif))
		(RS10 (equal new_gf_bf 'probablement_oui) (setq bilan_rencontre 'neutre))
		(RS11 (equal new_gf_bf 'oui) (setq bilan_rencontre 'positif))
		(RS12 (equal bagarre_soiree 'baston_violente) (question_fait 'implication *BF*))
		(RS13 (equal bagarre_soiree 'petite_embrouille) (question_fait 'implication *BF*))
		(RS14 (equal bagarre_soiree 'non) (and (setq bilan_violence 'positif) (setq implication 'NN)))
		(RS15 (and (equal bagarre_soiree 'baston_violente) (equal implication 'vous)) (setq bilan_violence 'negatif))
		(RS16 (and (equal bagarre_soiree 'baston_violente) (equal implication 'amis)) (setq bilan_violence 'negatif))
		(RS17 (and (equal bagarre_soiree 'baston_violente) (equal implication 'inconnus)) (setq bilan_violence 'neutre))
		(RS18 (and (equal bagarre_soiree 'petite_embrouille) (equal implication 'vous)) (setq bilan_violence 'negatif))
		(RS19 (and (equal bagarre_soiree 'petite_embrouille) (equal implication 'amis)) (setq bilan_violence 'neutre))
		(RS20 (and (equal bagarre_soiree 'petite_embrouille) (equal implication 'inconnus)) (setq bilan_violence 'positif))
		(RS21 (and (equal bilan_violence 'positif) (equal bilan_rencontre 'positif)) (setq bilan_socialisation 'positif))
		(RS22 (and (equal bilan_violence 'positif) (equal bilan_rencontre 'neutre)) (setq bilan_socialisation 'positif))
		(RS23 (and (equal bilan_violence 'positif) (equal bilan_rencontre 'negatif)) (setq bilan_socialisation 'neutre))
		(RS24 (and (equal bilan_violence 'neutre) (equal bilan_rencontre 'positif)) (setq bilan_socialisation 'positif))
		(RS25 (and (equal bilan_violence 'neutre) (equal bilan_rencontre 'neutre)) (setq bilan_socialisation 'neutre))
		(RS26 (and (equal bilan_violence 'neutre) (equal bilan_rencontre 'negatif)) (setq bilan_socialisation 'negatif))
		(RS27 (and (equal bilan_violence 'negatif) (equal bilan_rencontre 'positif)) (setq bilan_socialisation 'neutre))
		(RS28 (and (equal bilan_violence 'negatif) (equal bilan_rencontre 'neutre)) (setq bilan_socialisation 'negatif))
		(RS29 (and (equal bilan_violence 'negatif) (equal bilan_rencontre 'negatif)) (setq bilan_socialisation 'negatif))
		(RS30 (equal rencontre 'non) (and (setq bilan_rencontre 'negatif) (setq type_rencontre 'NN) (setq coucher_avec 'NN) (setq new_gf_bf 'NN)))

		;divertissement
		(RD1 (equal perdu_qqc 'non) (and (setq valeur_perte 'aucune) (setq valeur_perte_argent 'NN) (setq valeur_perte_sentiment 'NN) (setq radin 'NN) ))
		(RD2 (equal perdu_qqc 'oui) (and (question_fait 'valeur_perte_sentiment *bf*) (question_fait 'valeur_perte_argent *bf*)))
		(RD3 (and (equal radin 'un_peu) (equal valeur_perte_argent 'enorme)) (setq valeur_perte_argent 'grande))
		(RD4 (and (equal radin 'oui) (equal valeur_perte_argent 'enorme)) (setq valeur_perte_argent 'grande))
		(RD5 (and (equal radin 'oui) (equal valeur_perte_argent 'grande)) (setq valeur_perte_argent 'petite))
		(RD6 (and (equal radin 'oui) (equal valeur_perte_argent 'petite)) (setq valeur_perte_argent 'aucune))
		(RD7 (and (equal valeur_perte_argent 'enorme) (equal valeur_perte_sentiment 'enorme)) (setq valeur_perte 'enorme))
		(RD8 (and (equal valeur_perte_argent 'enorme) (equal valeur_perte_sentiment 'grande)) (setq valeur_perte 'enorme))
		(RD9 (and (equal valeur_perte_argent 'enorme) (equal valeur_perte_sentiment 'petite)) (setq valeur_perte 'grande))
		(RD10 (and (equal valeur_perte_argent 'enorme) (equal valeur_perte_sentiment 'aucune)) (setq valeur_perte 'petite))
		(RD11 (and (equal valeur_perte_argent 'grande) (equal valeur_perte_sentiment 'enorme)) (setq valeur_perte 'enorme))
		(RD12 (and (equal valeur_perte_argent 'grande) (equal valeur_perte_sentiment 'grande)) (setq valeur_perte 'enorme))
		(RD13 (and (equal valeur_perte_argent 'grande) (equal valeur_perte_sentiment 'petite)) (setq valeur_perte 'grande))
		(RD14 (and (equal valeur_perte_argent 'grande) (equal valeur_perte_sentiment 'aucune)) (setq valeur_perte 'petite))
		(RD15 (and (equal valeur_perte_argent 'petite) (equal valeur_perte_sentiment 'enorme)) (setq valeur_perte 'enorme))
		(RD16 (and (equal valeur_perte_argent 'petite) (equal valeur_perte_sentiment 'grande)) (setq valeur_perte 'grande))
		(RD17 (and (equal valeur_perte_argent 'petite) (equal valeur_perte_sentiment 'petite)) (setq valeur_perte 'petite))
		(RD18 (and (equal valeur_perte_argent 'petite) (equal valeur_perte_sentiment 'aucune)) (setq valeur_perte 'aucune))
		(RD19 (and (equal valeur_perte_argent 'aucune) (equal valeur_perte_sentiment 'enorme)) (setq valeur_perte 'grande))
		(RD20 (and (equal valeur_perte_argent 'aucune) (equal valeur_perte_sentiment 'grande)) (setq valeur_perte 'petite))
		(RD21 (and (equal valeur_perte_argent 'aucune) (equal valeur_perte_sentiment 'petite)) (setq valeur_perte 'aucune))
		(RD22 (and (equal valeur_perte_argent 'aucune) (equal valeur_perte_sentiment 'aucune)) (setq valeur_perte 'aucune))
		(RD23 (and (equal valeur_perte 'enorme) (equal dance_du_limousin 'plusieurs)) (setq bilan_divertissement 'neutre))
		(RD24 (and (equal valeur_perte 'enorme) (equal dance_du_limousin 'une)) (setq bilan_divertissement 'negatif))
		(RD25 (and (equal valeur_perte 'enorme) (equal dance_du_limousin 'aucune)) (setq bilan_divertissement 'negatif))
		(RD26 (and (equal valeur_perte 'grande) (equal dance_du_limousin 'plusieurs)) (setq bilan_divertissement 'neutre))
		(RD27 (and (equal valeur_perte 'grande) (equal dance_du_limousin 'une)) (setq bilan_divertissement 'neutre))
		(RD28 (and (equal valeur_perte 'grande) (equal dance_du_limousin 'aucune)) (setq bilan_divertissement 'negatif))
		(RD29 (and (equal valeur_perte 'petite) (equal dance_du_limousin 'plusieurs)) (setq bilan_divertissement 'positif))
		(RD30 (and (equal valeur_perte 'petite) (equal dance_du_limousin 'une)) (setq bilan_divertissement 'neutre))
		(RD31 (and (equal valeur_perte 'petite) (equal dance_du_limousin 'aucune)) (setq bilan_divertissement 'neutre))
		(RD32 (and (equal valeur_perte 'aucune) (equal dance_du_limousin 'plusieurs)) (setq bilan_divertissement 'positif))
		(RD33 (and (equal valeur_perte 'aucune) (equal dance_du_limousin 'une)) (setq bilan_divertissement 'positif))
		(RD34 (and (equal valeur_perte 'aucune) (equal dance_du_limousin 'aucune)) (setq bilan_divertissement 'neutre))

		;nourriture
		(RN1 (equal nourriture_presence 'non) (and (setq bilan_nourriture 'negatif) (setq quantite_nourriture_debut 'NN) (setq quantite_nourriture_fin 'NN) (setq rapport_quantite_succes 'NN) (setq gout_personnel 'NN) ))
		(RN2 (equal nourriture_presence 'oui) (and (question_fait 'quantite_nourriture_debut *bf*) (question_fait 'quantite_nourriture_fin *bf*) (question_fait 'gout_personnel *bf*)))
		(RN3 (and (equal quantite_nourriture_debut 'beaucoup) (equal quantite_nourriture_fin 'beaucoup)) (setq rapport_quantite_succes 'nourriture_pas_bonne))
		(RN4 (and (equal quantite_nourriture_debut 'beaucoup) (equal quantite_nourriture_fin 'normal)) (setq rapport_quantite_succes 'trop_nourriture))
		(RN5 (and (equal quantite_nourriture_debut 'beaucoup) (equal quantite_nourriture_fin 'peu)) (setq rapport_quantite_succes 'parfait))
		(RN6 (and (equal quantite_nourriture_debut 'beaucoup) (equal quantite_nourriture_fin 'rien)) (setq rapport_quantite_succes 'trop_affame))
		(RN7 (and (equal quantite_nourriture_debut 'normal) (equal quantite_nourriture_fin 'normal)) (setq rapport_quantite_succes 'nourriture_pas_bonne))
		(RN8 (and (equal quantite_nourriture_debut 'normal) (equal quantite_nourriture_fin 'peu)) (setq rapport_quantite_succes 'parfait))
		(RN9 (and (equal quantite_nourriture_debut 'normal) (equal quantite_nourriture_fin 'rien)) (setq rapport_quantite_succes 'parfait))
		(RN10 (and (equal quantite_nourriture_debut 'peu) (equal quantite_nourriture_fin 'peu)) (setq rapport_quantite_succes 'nourriture_pas_bonne))
		(RN11 (and (equal quantite_nourriture_debut 'peu) (equal quantite_nourriture_fin 'rien)) (setq rapport_quantite_succes 'pas_assez_�_manger))
		(RN12 (and (equal rapport_quantite_succes 'nourriture_pas_bonne) (equal gout_personnel 'tres_bonne)) (setq bilan_nourriture 'positif))
		(RN13 (and (equal rapport_quantite_succes 'nourriture_pas_bonne) (equal gout_personnel 'correcte)) (setq bilan_nourriture 'neutre))
		(RN14 (and (equal rapport_quantite_succes 'nourriture_pas_bonne) (equal gout_personnel 'bofbof)) (setq bilan_nourriture 'negatif))
		(RN15 (and (equal rapport_quantite_succes 'nourriture_pas_bonne) (equal gout_personnel 'mauvaise)) (setq bilan_nourriture 'negatif))
		(RN16 (and (equal rapport_quantite_succes 'trop_nourriture) (equal gout_personnel 'tres_bonne)) (setq bilan_nourriture 'positif))
		(RN17 (and (equal rapport_quantite_succes 'trop_nourriture) (equal gout_personnel 'correcte)) (setq bilan_nourriture 'neutre))
		(RN18 (and (equal rapport_quantite_succes 'trop_nourriture) (equal gout_personnel 'bofbof)) (setq bilan_nourriture 'negatif))
		(RN19 (and (equal rapport_quantite_succes 'trop_nourriture) (equal gout_personnel 'mauvaise)) (setq bilan_nourriture 'negatif))
		(RN20 (and (equal rapport_quantite_succes 'parfait) (equal gout_personnel 'tres_bonne)) (setq bilan_nourriture 'positif))
		(RN21 (and (equal rapport_quantite_succes 'parfait) (equal gout_personnel 'correcte)) (setq bilan_nourriture 'positif))
		(RN22 (and (equal rapport_quantite_succes 'parfait) (equal gout_personnel 'bofbof)) (setq bilan_nourriture 'neutre))
		(RN23 (and (equal rapport_quantite_succes 'parfait) (equal gout_personnel 'mauvaise)) (setq bilan_nourriture 'negatif))
		(RN24 (and (equal rapport_quantite_succes 'trop_affame) (equal gout_personnel 'tres_bonne)) (setq bilan_nourriture 'negatif))
		(RN25 (and (equal rapport_quantite_succes 'trop_affame) (equal gout_personnel 'correcte)) (setq bilan_nourriture 'negatif))
		(RN26 (and (equal rapport_quantite_succes 'trop_affame) (equal gout_personnel 'bofbof)) (setq bilan_nourriture 'neutre))
		(RN27 (and (equal rapport_quantite_succes 'trop_affame) (equal gout_personnel 'mauvaise)) (setq bilan_nourriture 'positif))
		(RN28 (and (equal rapport_quantite_succes 'pas_assez_�_manger) (equal gout_personnel 'tres_bonne)) (setq bilan_nourriture 'negatif))
		(RN29 (and (equal rapport_quantite_succes 'pas_assez_�_manger) (equal gout_personnel 'correcte)) (setq bilan_nourriture 'negatif))
		(RN30 (and (equal rapport_quantite_succes 'pas_assez_�_manger) (equal gout_personnel 'bofbof)) (setq bilan_nourriture 'neutre))
		(RN31 (and (equal rapport_quantite_succes 'pas_assez_�_manger) (equal gout_personnel 'mauvaise)) (setq bilan_nourriture 'positif))

		;solution
		(RF1 (and (equal bilan_musique 'positif) (equal bilan_boisson 'positif)) (setq solution 'bonne_soiree))
		(RF2 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'positif)) (setq solution 'bonne_soiree))
		(RF3 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'positif)) (setq solution 'bonne_soiree))
		(RF4 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'neutre)) (setq solution 'bonne_soiree))
		(RF5 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'negatif)) (setq solution 'soiree_mitigee))
		(RF6 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'neutre)) (setq solution 'soiree_mitigee))
		(RF7 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'negatif)) (setq solution 'mauvaise_soiree))
		(RF8 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'positif)) (setq solution 'bonne_soiree))
		(RF9 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'negatif)) (setq solution 'soiree_mitigee))
		(RF10 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'neutre)) (setq solution 'soiree_mitigee))
		(RF11 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'neutre)) (setq solution 'mauvaise_soiree))
		(RF12 (and (equal bilan_musique 'positif) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'negatif)) (setq solution 'mauvaise_soiree))
		(RF13 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'positif)) (setq solution 'bonne_soiree))
		(RF14 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'positif)) (setq solution 'bonne_soiree))
		(RF15 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'neutre)) (setq solution 'bonne_soiree))
		(RF16 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'negatif)) (setq solution 'soiree_mitigee))
		(RF17 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'neutre) (equal bilan_nourriture 'positif)) (setq solution 'soiree_mitigee))
		(RF18 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'neutre) (equal bilan_nourriture 'negatif)) (setq solution 'mauvaise_soiree))
		(RF19 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'neutre) (equal bilan_nourriture 'neutre)) (setq solution 'soiree_mitigee))
		(RF20 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'negatif)) (setq solution 'soiree_mitigee))
		(RF21 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'neutre)) (setq solution 'soiree_mitigee))
		(RF22 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'positif) (equal bilan_socialisation 'neutre) (equal bilan_divertissement 'positif)) (setq solution 'bonne_soiree))
		(RF23 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'positif) (equal bilan_divertissement 'positif)) (setq solution 'bonne_soiree))
		(RF24 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'positif) (equal bilan_divertissement 'neutre)) (setq solution 'soiree_mitigee))
		(RF25 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'positif) (equal bilan_divertissement 'negatif)) (setq solution 'soiree_mitigee))
		(RF26 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'neutre)) (setq solution 'soiree_mitigee))
		(RF27 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'positif)) (setq solution 'soiree_mitigee))
		(RF28 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'negatif)) (setq solution 'mauvaise_soiree))
		(RF29 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'positif) (equal bilan_nourriture 'neutre)) (setq solution 'soiree_mitigee))
		(RF30 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'neutre) (equal bilan_nourriture 'positif)) (setq solution 'soiree_mitigee))
		(RF31 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'neutre) (equal bilan_nourriture 'neutre)) (setq solution 'mauvaise_soiree))
		(RF32 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'neutre) (equal bilan_nourriture 'negatif)) (setq solution 'mauvaise_soiree))
		(RF33 (and (equal bilan_musique 'neutre) (equal bilan_boisson 'neutre) (equal bilan_socialisation 'negatif) (equal bilan_divertissement 'negatif)) (setq solution 'mauvaise_soiree))
		(RF34 (or (equal bilan_musique 'negatif) (equal bilan_boisson 'negatif) ) (setq solution 'mauvaise_soiree))
	)
)

;;;
;;;		DEBUT
;;;	fonctions de service
;;;       r�gles
;;;

;;retourne le nom de la r�gles
(defun n_r (regle)
	(car regle)
)

;;retourne la condition d'une regle
(defun cd_r (regle)
	(cadr regle)
)

;;retourne la cons�quence d'une regle
(defun cq_r (regle)
	(caddr regle)
)

;;retourne vrai si les conditions d'une regle sont valid�es
(defun cd_r? (regle)
	(eval (cd_r regle))
)

;;retourne la liste des noms des r�gles applicables � partir d'une base de r�gle
(defun lr_app (base_regles)
	(let ((lister_app NIL))
		(dolist (r base_regles lister_app)
			(if (cd_r? r) (push (n_r r) lister_app))
		)
	)
)

;;retourne la liste des noms des r�gles applicables et non appliqu�es
(defun lr_candidates (lr_appliquees lr_applicables)
	(let ((res nil))
		(dolist (r lr_applicables res)
			(if (not (member r lr_appliquees)) (push r res))
		)
	)
)

;;retourne le nom de la premi�re r�gle applicable de plus petit niveau
(defun premiere_regle_app (liste_noms_regles base_faits base_regles)
	(let ((i 20) (res NIL))
		(dolist (nom_regle liste_noms_regles res)
			(let ((lvl (lvl_cd (cd_r (assoc nom_regle base_regles))  base_faits)))
				(cond
					((<= lvl i) (setq i lvl) (setq res nom_regle))
				)
			)
		)
	)
)

;;retourne le "niveau de la condition" (egal au plus petit niveau des faits utilis�s dans la condition)
(defun lvl_cd (conditions base_faits)
	(cond
		((NULL conditions) 5)
		((equal (car conditions) 'equal) (lvl_f (cadr conditions) base_faits))
		((equal (car conditions) 'not) (lvl_f (cadr (cadr conditions)) base_faits))
		((equal (car conditions) 'and) (lvl_cd (cdr conditions) base_faits))
		((equal (car conditions) 'or) (lvl_cd (cdr conditions) base_faits))
		((listp (car conditions)) (min (lvl_cd (car conditions) base_faits) (lvl_cd (cdr conditions) base_faits)))
	)
)	



;;affichage de la base des r�gles
(defun affichage_br (base_regles)
	(format T "~%~%d�but de la base des r�gles ~%~%")
	(dolist (regle base_regles T)
		(format T "~%R�gle n�:~A~%condition(s):~%~A~%cons�quence(s):~%~A~%" (n_r regle) (cd_r regle) (cq_r regle))
	)
	(format T "~%fin de la base des r�gles ~%~%")
)

;;;
;;;    r�gles
;;; fonctions de service
;;;     FIN



;;;interroger l'utilisateur
(defun question_fait (nom_fait base_faits)
	(format T "~%~%question � propos de ~A~%" nom_fait)
	(format T "~%~A ~%" (q_f nom_fait base_faits))
	(let ((possibilites (vp_f nom_fait base_faits)) (i 0))
		(dolist (p possibilites T)
			(setq i (+ 1 i))
			(if (not (equal p 'NR)) (format T "choix n�~A : ~A ~%" i p))
		)
	(format T "~%Votre r�ponse ? ~%")
	(setq i (read))
	(format T "Vous avez r�pondu : ~A~%~%" (set nom_fait (reponse_possibilites possibilites i)))
	)
)

;;retourne la bonne r�ponse
(defun reponse_possibilites (possibilites i)
(dotimes (x (- i 1) (car possibilites))
	(setq possibilites (cdr possibilites))
)
)


;;;
;;;		DEBUT
;;;		Moteur d'inf�rence
;;;		V1
;;;
(defun moteur_inf (base_regles base_faits liste_regles_appliquees)
	(let* ((regles_applicables (lr_app base_regles)) (regles_candidates (lr_candidates liste_regles_appliquees regles_applicables)))
	(cond 
		((not (equal solution 'NR)) (format T "~%***************************~%je pense que vous avez pass� une: ~A~%***************************~%" solution))
		((equal regles_candidates NIL) 
			(format T "~%***************************~%Hum, int�ressant, continuez votre histoire...~%***************************~%")
			(question_fait (car (premier_fait_NR base_faits)) base_faits)
			(moteur_inf base_regles base_faits liste_regles_appliquees)
		)
		((not (equal regles_candidates NIL))
			(let ((nom_regle_appliquee (premiere_regle_app regles_candidates base_faits base_regles)))
			(format T "~%***************************~%Hum, je vois, je vois.~%Laissez moi r�fl�chir...~%application de la regle : ~A~%***************************~%" nom_regle_appliquee)
			(eval (cq_r (assoc nom_regle_appliquee base_regles)))
			(push nom_regle_appliquee *explications*)
			(moteur_inf base_regles base_faits (push nom_regle_appliquee liste_regles_appliquees))
			)
		)
		(T "Je ne sais pas quoi faire")
	)
	)
)

;;;
;;;		Moteur d'inf�rence
;;;		FIN
;;;


;;;
;;;		DEBUT
;;;		fonctions de service
;;;		utilitaire
;;;

;cr�ation de la base des explications
(setq *base_explications* 
	'(
		;explication musique
		(RM1 "Sans son, difficile de faire une soir�e.")
		(RM3 "Vous n'aimez pas la musique et le volume est trop fort ou trop faible, j'en d�duis donc que la qualit� du son est mauvaise")
		(RM4 "Vous aimez la musique mais le volume est trop fort ou trop faible, j'en d�duis donc que la qualit� du son est moyenne")
		(RM5 "Vous n'aimez pas la musique mais le volume est parfait, j'en d�duis donc que la qualit� du son est moyenne")
		(RM6 "Vous aimez la musique et le volume est parfait, j'en d�duis donc que la qualit� du son est parfaite.")
		(RM7 "Vous n'avez pas dans� de la soir�e, et tout le monde ne dansait pas, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait mauvaise") 
		(RM8 "Vous n'avez pas dans� de la soir�e, mais tout le monde dansait, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait moyenne. La piste n'attendait que vous !") 
		(RM9 "Vous n'avez dans� que par moments, et personne d'autre ne dansait, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait mauvaise. Bien essay� quand m�me!") 
		(RM10 "Vous n'avez dans� que par moments, et seuls les gar�ons ou les filles vous ont rejoint, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait moyenne. Vive la mixit�") 
		(RM12 "Vous n'avez dans� que par moments, mais tout le monde dansait, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait bonne. Vous n'�tiez peut-�tre pas en grande forme ce soir l�!") 
		(RM13 "Vous avez dans� toute la soir�e, mais vous �tiez bien le seul, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait moyenne. Solitude ...!") 
		(RM14 "Vous avez dans� toute la soir�e, mais seuls les garcons ou les filles vous ont rejoint, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait bonne. Vous �tiez 'dabei', pensez � plus de mixit�!") 
		(RM15 "Vous avez dans� toute la soir�e, et tout le monde a r�pondu pr�sent, j'en d�duis donc que l'ambiance 'dancefloor', si vous me permettez cette expression, �tait excellente. Vous �tiez 'dabei', et les autres aussi!") 
		(RM16 "Une qualit� de son ainsi qu'une ambiance 'dancefloor' mauvaise, j'en d�duis donc que le bilan musical de la soir�e �tait tr�s mauvais. Rien pour remonter le niveau, dommage!") 
		(RM17 "Une qualit� de son mauvaise et une ambiance 'dancefloor' moyenne, j'en d�duis donc que le bilan musical de la soir�e �tait mauvais. Mais que fait le DJ?!") 
		(RM18 "Une qualit� de son ainsi qu'une ambiance 'dancefloor' moyenne, j'en d�duis donc que le bilan musical de la soir�e �tait moyen. C'�tait pas top, mais �a aurait pu �tre pire!") 
		(RM19 "Une qualit� de son bonne mais une ambiance 'dancefloor' mauvaise, j'en d�duis donc que le bilan musical de la soir�e �tait mauvais. Manque de motivation ?!") 
		(RM20 "Une qualit� de son mauvaise mais une ambiance 'dancefloor' bonne, j'en d�duis donc que le bilan musical de la soir�e �tait mauvais. Mais que fait le DJ?!") 
		(RM21 "Une qualit� de son bonne et une ambiance 'dancefloor' neutre, j'en d�duis donc que le bilan musical de la soir�e �tait bon. Vous m'inviterez la prochaine fois?") 
		(RM22 "Une qualit� de son neutre et une ambiance 'dancefloor' bonne, j'en d�duis donc que le bilan musical de la soir�e �tait bon. Vous m'inviterez la prochaine fois?") 
		(RM23 "Une qualit� de son bonne et une ambiance 'dancefloor' bonne, j'en d�duis donc que le bilan musical de la soir�e �tait bon. Vous m'inviterez la prochaine fois?") 
		(RM24 "Une qualit� de son bonne et une ambiance 'dancefloor' excellente, j'en d�duis donc que le bilan musical de la soir�e �tait tr�s bon. Difficile de faire mieux!") 
		(RM25 "Une qualit� de son neutre mais une ambiance 'dancefloor' excellente, j'en d�duis donc que le bilan musical de la soir�e �tait bon. Vous m'inviterez la prochaine fois?") 
		(RM26 "Une qualit� de son mauvaise et une ambiance 'dancefloor' excellente, j'en d�duis donc que le bilan musical de la soir�e �tait moyen. Grosse motivation, mais un dj pas au rendez-vous.")
		 
		;explication alcool
		(RB1 "Sans alcool, la f�te est moins folle.")
		(RB4 "Il y avait beaucoup d'alcool au d�but, mais il en restait beaucoup � la fin, j'en d�duis donc que les gens n'ont pas bu. Un verre ou deux rend joyeux!")
		(RB5 "Il y avait beaucoup d'alcool au d�but et il en restait � la fin, j'en d�duis donc que les gens ont bu mod�rement. Joyeux oui, bourr� non!")
		(RB5 "Il y avait beaucoup d'alcool au d�but et il en restait un peu � la fin, j'en d�duis donc que les gens ont bu mod�rement. Joyeux oui, bourr� non!")
		(RB7 "Il y avait beaucoup d'alcool au d�but et il ne restait rien � la fin, j'en d�duis donc que les gens ont trop bu. L'abbus d'alcool est dangereux pour la sant�!")
		(RB8 "Il y avait beaucoup d'alcool au d�but et il ne restait rien � la fin, j'en d�duis donc que les gens ont trop bu. L'abbus d'alcool est dangereux pour la sant�!")
		(RB8 "Il y avait une quantit� raisonnable d'alcool au d�but mais il en restait autant � la fin, j'en d�duis donc que les gens n'ont pas bu. Un verre ou deux rend joyeux!")
		(RB9 "Il y avait une quantit� raisonnable d'alcool au d�but et il en restait un peu � la fin, j'en d�duis donc que les gens ont bu mod�rement. Joyeux oui, bourr� non!")
		(RB10 "Il y avait une quantit� raisonnable d'alcool au d�but mais il ne restait plus rien � la fin, j'en d�duis donc que les gens ont trop bu. L'abbus d'alcool est dangereux pour la sant�!")
		(RB11 "Il y avait peu d'alcool au d�but et il en restait autant � la fin, j'en d�duis donc que les gens n'ont pas bu.  Un verre ou deux rend joyeux!")
		(RB12 "Il y avait peu d'alcool au d�but et il ne restait plus rien � la fin, j'en d�duis donc que les gens ont bu mod�rement.  Joyeux oui, bourr� non!")
		(RB13 "Vous n'avez pas vomi et tout le monde �tait joyeux, point positif pour une soir�e r�ussie !")
		(RB14 "Vous n'avez pas vomi mais tout le monde avait trop bu. Attention, trop d'alcool nuit � la soir�e!")
		(RB15 "Vous n'avez pas vomi et aucune goutte d'alcool n'a �t� aval�e. Peut-�tre un peu trop s�rieux, non ?!")
		(RB16 "Vous avez vomi en d�but de soir�e. En plus d'�tre dangereux, un tel comportement vous g�che la soir�e!")
		(RB17 "Vous avez vomi au milieu de la soir�e, mais tout le monde �tait joyeux. Prenez exemple sur vos amis, vous profiterez aussi de la fin de soir�e!")
		(RB19 "Vous avez vomi au milieu de la soir�e, mais les autres n'ont bu aucune goutte d'alcool. Vous �tiez � contre courant dans cette soir�e!")
		(RB18 "Vous avez vomi au milieu de la soir�e et tout le monde avait trop bu. Attention, trop d'alcool nuit � la soir�e!")
		(RB20 "Vous avez vomi en fin de soir�e et tout le monde �tait joyeux. Super soir�e, qui aurait pu mieux se terminer!")
		(RB21 "Vous avez vomi en fin de soir�e et tout le monde avait trop bu. Vous avez fait comme tout le monde, mais attention trop d'alcool peut g�cher une soiree!")
		(RB22 "Vous avez vomi en fin de soir�e mais personne d'autre n'a bu d'alcool. Vous �tiez � contre courant dans cette soir�e!")

		;explication socialisation
		(RS3 "Faire une rencontre amicale est toujours positif!")
		(RS4 "Pas de rencontre exceptionnel, �a aurait pu �tre mieux, �a aurait pu �tre pire!")
		(RS5 "Une rencontre d�cevante ne fait jamais plaisir!")
		(RS6 "Vous avez flirt� avec une personne pendant la soir�e et vous avez termin� dans son lit, peut-�tre une nouvelle relation � l'horizon ;o ")
		(RS7 "Petit flirt qui se transforme en nouvelle relation, � vous de faire le choix!")
		(RS8 "Petit flirt qui en restera l�. Relation d'un soir.")
		(RS9 "Relation d'un soir, d�sespoir")
		(RS10 "Relation d'une nuit. C'est d�j� pas si mal.")
		(RS11 "Nouveau couple form�! Je prends les paris sur la dur�e!")
		(RS14 "Sans violence, la soir�e ne peut que mieux se passer")
		(RS15 "Vous avez �t� impliqu� dans une violente dispute. La soir�e a du en prendre un coup")
		(RS16 "Vos amis ont �t� impliqu�s dans une violente dispute. La soir�e a du en prendre un coup")
		(RS17 "Des inconnus ont �t� impliqu�s dans une violente dispute. Pas de quoi se r�jouir, mais allez savoir ce qui s'est r�ellement pass�...")
		(RS18 "Vous avez �t� impliqu� dans une petite dispute. La soir�e a du en prendre un coup.")
		(RS19 "Vos amis ont �t� impliqu�s dans une petite dispute. Vous vous en seriez bien pass�, mais ce n'est pas grave et c'est bien le principal.")
		(RS20 "Des inconnus ont �t� impliqu�s dans une petite dispute. Ca arrive tout le temps, continuez � vous amuser!")
		(RS21 "Soir�e sans violence et rencontre heureuse, que du bonheur")
		(RS22 "Soir�e sans violence et sans rencontre particuli�re, �a aurait pu �tre mieux, mais �a aurait surtout pu �tre pire!")
		(RS23 "Rencontre maheureuse, mais soir�e sans violence, c'est d�j� �a.")
		(RS24 "Bilan sur la violence mitig�e, mais rencontre heureuse, que du bonheur!")
		(RS25 "Bilan sur la violence mitig�e, sans rencontre particuli�re, �a aurait pu �tre mieux, mais �a aurait surtout pu �tre pire!")
		(RS26 "Bilan sur la violence mitig�e, mais rencontre d�sagr�able. Vraiment pas top les invit�s!")
		(RS27 "Rencontre agr�able, mais quelle violence! �a aurait pu �tre pire, mais �a aurait surtout pu �tre mieux!")
		(RS28 "Pas de rencontre particuli�re, mais quelle violence! Vraiment pas top les invit�s!")
		(RS29 "Rencontre d�sagr�able et quelle violence! Ne faites plus de soir�e avec les m�mes invit�s!")
		(RS30 "Pas de rencontre ?!? Soit on ne vous appr�cie pas, soit vous n'appr�ciez pas les autres. Rassurez-moi ? Vous n'avez pas fait la f�te seul(e)?")

		;explication diverstissement
		(RD1 "Vous n'avez rien perdu, c'est d�j� �a de gagn�.")
		(RD3 "Votre perte financi�re est soit-disant �norme, mais vous �tes l�g�rement � cheval sur l'argent. Je pense que 'grande perte' conviendrait mieux, non?")
		(RD4 "Votre perte financi�re est soit-disant �norme, mais vous �tes � cheval sur l'argent. Je pense que 'grande perte' conviendrait mieux, non?")
		(RD5 "Votre perte financi�re est soit-disant grande, mais vous �tes � cheval sur l'argent. Je pense que 'petite perte' conviendrait mieux, non?")
		(RD6 "Votre perte financi�re est soit-disant petite, mais vous �tes � cheval sur l'argent. Je pense que 'aucune perte' conviendrait mieux, non?")
		(RD7 "Perte financi�re et perte sentimentale �normes. Vous vous en souviendrez longtemps, et en mal!")
		(RD8 "Perte financi�re �norme et grosse perte sentimentale. Vous vous en souviendrez longtemps, et en mal!")
		(RD9 "Perte financi�re �norme et petite perte sentimentale. Vous vous en souviendrez...")
		(RD10 "Perte financi�re �norme mais aucune perte sentimentale. Vous devriez vite oublier...")
		(RD11 "Grosse Perte financi�re et perte sentimentale �norme. Vous vous en souviendrez longtemps, et en mal!")
		(RD12 "Grosse Perte financi�re et grosse perte sentimentale. Vous vous en souviendrez longtemps, et en mal!")
		(RD13 "Grosse Perte financi�re et petite perte sentimentale. Vous vous en souviendrez...")
		(RD14 "Grosse Perte financi�re et aucune perte sentimentale. Vous devriez vite oublier...")
		(RD15 "Petite Perte financi�re et �norme perte sentimentale. Vous vous en souviendrez longtemps, et en mal!")
		(RD16 "Petite Perte financi�re et grande perte sentimentale. Vous vous en souviendrez...")
		(RD17 "Petite Perte financi�re et petite perte sentimentale. Vous devriez vite oublier...")
		(RD18 "Petite Perte financi�re et petite perte sentimentale. Admetez, vous avez d�j� oubli�!")
		(RD19 "Aucune Perte financi�re et �norme perte sentimentale. Vous vous en souviendrez...")
		(RD20 "Aucune Perte financi�re et grande perte sentimentale. Vous devriez vite oublier...")
		(RD21 "Aucune Perte financi�re et petite perte sentimentale. Admetez, vous avez d�j� oubli�!")
		(RD22 "Aucune Perte financi�re et aucune perte sentimentale. Admetez, vous avez d�j� oubli�!")
		(RD23 "Vous vous en souviendrez longtemps de votre perte, mais bon vous avez assist� � plusieurs danses du limousin. Relativisez!") 
		(RD24 "Vous vous en souviendrez longtemps de votre perte, et une seule danse du limousin durant la soir�e... Ambiance et chance n'�taient pas de la partie.") 
		(RD25 "Vous vous en souviendrez longtemps de votre perte, et aucune danse du limousin durant la soir�e... Ambiance et chance n'�taient vraiment pas de la partie.") 
		(RD26 "Vous vous en souviendrez de votre perte, mais bon vous avez assist� � plusieurs danses du limousin. Relativisez") 
		(RD27 "Vous vous en souviendrez de votre perte, mais vous avez assist� tout de m�me � une danse du limousin. Relativisez") 
		(RD28 "Vous vous en souviendrez de votre perte, et  aucune danse du limousin pour vous r�conforter. Ambiance et chance n'�tait pas de la partie.") 
		(RD29 "Vous devriez vite oublier ce que vous avez perdu, et vous avez assist� � plusieurs danses du limousin. Ambiance et chance au rendez-vous!") 
		(RD30 "Vous devriez vite oublier ce que vous avez perdu, et vous avez assist� � une danse du limousin. Ambiance et chance au rendez-vous!") 
		(RD31 "Vous devriez vite oublier ce que vous avez perdu, et vous n'avez assist� � aucune du limousin. Chance au rendez-vous, mais absence d'ambiance") 
		(RD32 "Vous n'avez rien perdu, ou presque, et vous avez assist� � plusieurs danses du limousin. Ambiance et chance au rendez-vous!") 
		(RD33 "Vous n'avez rien perdu, ou presque, et vous avez assist� � une danse du limousin. Ambiance et chance au rendez-vous!") 
		(RD34 "Vous n'avez rien perdu, ou presque, mais pas de danse du limousin pour enflammer la soir�e. Chance au rendez-vous, mais manque terrible d'ambiance")

		;explication nourriture
		(RN1 "Sans rien � se mettre sous la dent, il ne fallait pas avoir faim :x") 
		(RN3 "Beaucoup de nourriture au d�but, beaucoup de nourriture � la fin. Quelque chose me dit que ce n'�tait pas tr�s bon.") 
		(RN4 "Beaucoup de nourriture au d�but, de la nourriture � la fin. Il y en avait trop ! Attention au gaspillage.") 
		(RN5 "Beaucoup de nourriture au d�but, peu de la nourriture � la fin. Quantit� et qualit� au rendez-vous!") 
		(RN6 "Beaucoup de nourriture au d�but,  plus rien � la fin. La soir�e s'est d�roul� devant le buffet ?!?") 
		(RN7 "De la nourriture au d�but,  de la nourriture � la fin. Quelque chose me dit que ce n'�tait pas tr�s bon.") 
		(RN8 "De la nourriture au d�but,  peu de nourriture � la fin. Quantit� et qualit� au rendez-vous!") 
		(RN9 "De la nourriture au d�but,  plus rien � la fin. Quantit� et qualit� au rendez-vous!") 
		(RN10 "Peu de nourriture au d�but,  peu de nourriture � la fin. Quelque chose me dit que ce n'�tait pas tr�s bon.") 
		(RN11 "Peu de nourriture au d�but,  plus rien � la fin. Quelque chose me dit qu'il n'y en avait peut-�tre pas assez.") 
		(RN12 "Ce n'�tait pas tr�s bon, mais vous vous avez ador�. Vous aviez tout pour vous!") 
		(RN13 "Ce n'�tait pas tr�s bon, mais vous avez trouv� �a correct. C'est pas du 3 �toiles, mais c'est largement suffisant pour une soir�e!") 
		(RN14 "Ce n'�tait pas tr�s bon. Mais que fait le cuisinier ?!") 
		(RN15 "Ce n'�tait pas tr�s bon, et vous avez vraiment detest�. Mais que fait le cuisinier ?!") 
		(RN16 "Il y avait trop � manger, mais vous avez ador�. Cusinier g�n�reux et talentueux!") 
		(RN17 "Il y avait trop � manger, mais vous avez trouv� �a correct. C'est pas du 3 �toiles, mais c'est largement suffisant pour une soir�e.") 
		(RN18 "Il y avait trop � manger. Du temps de pr�paration perdu pour les cuisiniers") 
		(RN19 "Il y avait trop � manger, et vous n'avez pas aim�. Mais que fait le cuisinier ?!") 
		(RN20 "Quantit� et qualit� au rendez-vous, et vous avez ador�. Cusinier g�n�reux et talentueux!") 
		(RN21 "Quantit� et qualit� au rendez-vous, et vous avez trouv� �a correct. Cusinier g�n�reux et talentueux!") 
		(RN22 "Quantit� et qualit� au rendez-vous. C'est pas du 3 �toiles, mais c'est largement suffisant pour une soir�e.") 
		(RN23 "Quantit� et qualit� au rendez-vous, mais vous, vous avez detest�. Vos go�ts ne correspondent pas avec ceux des autres, dommage!") 
		(RN24 "Vous avez ador� la nourriture, mais il n'y en avait pas assez. Dommage, dommage!") 
		(RN25 "Vous avez trouv� la nourriture correcte, mais il n'y en avait pas assez. Tant pis!") 
		(RN26 "Il n'y en avait pas assez. On ne vient pas que pour manger apr�s tout!") 
		(RN27 "Il n'y en avait pas assez, mais vous avez detest�, alors ce n'est pas plus mal!") 
		(RN28 "Vous avez ador� la nourriture, mais il n'y en avait pas assez. Dommage, dommage!") 
		(RN29 "Vous avez trouv� la nourriture correcte, mais il n'y en avait pas assez. Tant pis!") 
		(RN30 "Il n'y en avait pas assez. On ne vient pas que pour manger apr�s tout!") 
		(RN31 "Il n'y en avait pas assez, mais vous avez detest�, alors ce n'est pas plus mal!")  
		 
		;explication solution
		(RF34 "Avec un bilan musical et/ou un bilan sur les boissons n�gatif, je crains que la soir�e ne soit rat�e...")
	)
)

(setq *explications* NIL)

(defun explication_rapide ()
	(format T "~%***************************~%Laissez-moi vous expliquer rapidement comment je suis arriv� � une telle conclusion...~%***************************~%")
	(if (not (equal bilan_musique 'NR)) (format T "Le bilan musical de la soir�e est : ~A~%" bilan_musique))
	(if (not (equal bilan_boisson 'NR)) (format T "Le bilan sur la boisson est : ~A~%" bilan_boisson))
	(if (not (equal bilan_socialisation 'NR)) (format T "Le bilan social de la soiree est : ~A~%" bilan_socialisation))
	(if (not (equal bilan_divertissement 'NR)) (format T "Le bilan du divertissement de la soiree est : ~A~%" bilan_divertissement))
	(if (not (equal bilan_nourriture 'NR)) (format T "Le bilan sur la nourriture est : ~A~%" bilan_nourriture))
	(if (not (equal solution 'NR)) (format T "~%Ce qui m'a pouss� � dire que votre soir�e �tait une : ~A~%~%" solution) (format T "~%Aucune explication � donner~%~%"))
	"J'en ai termin� avec mon explication rapide"
)

(defun explication_details ()
(format T "~%***************************~%Laissez-moi vous expliquer en d�tails comment je suis arriv� � une telle conclusion...~%***************************~%~%")
	(dolist (nom_expl (reverse *explications*) T)
		(let ((expl (assoc nom_expl *base_explications*)))
			(if expl (format T "~A~%~%" (cadr expl)))
		)
	)
	"J'en ai termin� avec mon explication d�taill�e"
)
	
;;;
;;;    utilitaires
;;; fonctions de service
;;;     FIN


;;;			D�but	
;;;		�x�cution suite au chargement
;;;
(initialisation_faits *BF*)
(affichage_bf *BF*)
(affichage_br *BR*)

(print '(Le fichier est bien charg�.))