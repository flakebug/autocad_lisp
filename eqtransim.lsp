; ¤¤¹©¤uµ{«Ø³y³¡
; ¥\¯à»¡©ú:
;	³]³Æ¹B¿é¸ô®|¼ÒÀÀ
;
; v0.1 - 2018/1/24 - ±ç·ç¤¸
; 	°ò¥»¥\¯à
;	1. ¿ï¨ú³]³Æª«¥ó
;	2. ¿ï¨ú³]³Æ«eºÝ¤¤¤ßÂI
;	3. ¿ï¨ú³]³Æ«áºÝ¤¤¤ßÂI
;	4. ¿ï¨ú³]³ÆÂH¶KÂI
;	5. ¿ï¨ú¸ô®|
;	¨Ï¥Îµ{¦¡«e¡A¥ý¥Îpline«ü¥Oµe¥X³]³Æ¹B¿é¸ô®|
;	ÂàÅs®É¡A­Y±Ä¥Îª½½ulineÃ¸»sªñ¦ü¸Ñ¡A¥²¶·ºÉ¥i¯àÁY¤p¨C¤@½u¬q¶ZÂ÷
;	ÂàÅs®É¡A­Y¨Ï¥Î©·½uarcÃ¸»s¡A«h»Ý¨Ï¥Îarc2segµ{¦¡©î¸Ñ¬°ª½½uªñ¦ü¸Ñ

(defun c:eqtransim	(/ pl)
  (print "=== program start ===")
  (setq equipment (entsel "\nselect equipment"))
  (setq equipment_front (getpoint "\nselect front center of equipment"))
  (setq	equipment_rear
	 (getpoint equipment_front
		   "\nselect rear center of equipment"
	 )
  )
  (setq	equipment_base
	 (getpoint equipment_front
		   "\nselect snap point of equipment"
	 )
  )
  (setq distance_of_equipment (distance equipment_front equipment_rear))
  (setq angle_of_equipment (rtd (angle equipment_front equipment_rear)))

  (setq points (list))
  (print "Select vehicle path")
  (and (setq pl (ssget '((0 . "LWPOLYLINE"))))
       (foreach	itm
		    (setq ptlist (mapcar 'cdr
					 (vl-remove-if-not
					   '(lambda (x)
					      (= (car x) 10)
					    )
					   (entget (ssname pl 0))
					 )
				 )
		    )
	 (setq points (append points (list itm)))
       )
  )

  (setq point_count (length points))
  (setq index 0)
  (repeat (- point_count 1)
    (print (nth index points))
    (setq srcp (nth index points))
    (setq dstp (nth (+ index 1) points))
    (setq distance_of_points (distance srcp dstp))
    (setq angle_of_path (rtd (angle srcp dstp)))

    (if	(= index 0)
      (progn
	(paste_equipment
	  equipment equipment_base angle_of_equipment angle_of_path srcp)
      )
    )

    (if	(> (/ distance_of_points distance_of_equipment) 2)
      (progn
	(setq incremental 0.2)
	(setq target_percentage 0)
	(setq srcpx (car srcp))
	(setq srcpy (cadr srcp))
	(setq dstpx (car dstp))
	(setq dstpy (cadr dstp))
	(setq xdiff (- dstpx srcpx))
	(setq ydiff (- dstpy srcpy))
	(while (<= target_percentage 1)
          (setq stpx (+ srcpx (* xdiff target_percentage)))
	  (setq stpy (+ srcpy (* ydiff target_percentage)))
	  (setq stpp (list stpx stpy))
	  (paste_equipment
	    equipment equipment_base angle_of_equipment	angle_of_path
	    stpp)
	  (setq target_percentage (+ target_percentage incremental))
	)
      )
      (progn

	(paste_equipment
	  equipment equipment_base angle_of_equipment angle_of_path dstp)
      )
    )

    (setq index (+ index 1))
    (princ)
  )


)

(defun rtd (a)
  (/ (* a 180.0) pi)
)

(defun paste_equipment (equipment	  equipment_base
			equipment_angle	  path_angle
			snap_point
		       )
  (print "\n*** Paste equipment")
  (setq angleofrotate (- (+ path_angle 180) equipment_angle))
  (command "rotate" equipment "" equipment_base angleofrotate)
  (command "copy" equipment "" equipment_base snap_point)
  (command "rotate"
	   equipment
	   ""
	   equipment_base
	   (- 0 angleofrotate)
  )
  (command "DELAY" 500)
)



;; 
;; Below program comes from http://cadxp.com/topic/14427-les-lisp-de-gile/
;; Author by Gille
;;

;;
;; Generation de polygones reguliers sur les Arcs , Cercles et Arcs dans Polylignes 
;;
;; Les XDatas sont copiees sur la nouvelle polyligne.
;;
;; Routine:  ARC2SEG  vers 1.03 par Gilles (gile) le 10 Nov 2008
;; Transforme les arcs, cercles et polyarcs en polylignes constituees de segments droits
;;
;; 1 - Correction du bug sur les arcs > 180 degres 
;; 
;; 2  Pour les XDatas, soit je copie tout dans la nouvelle entite soit, 
;; dans le cas des polylignes avec suppression, je ne fais que modifier les sommets de la polyligne. 
;; 
;; 3 - J'ai peaufine le traitement des largeurs, si la largeur de depart et la largeur de fin different,
;; celle des segments changera aussi proportionnellement. 
;; 
;; 4 - Generation soit sur le calque courant, soit sur le calque d'origine des objets (vs 1.03)
;;

;; 
;; Minimum Translation from French to US/English 
;; 

(defun c:Arc2Seg (/ arc2pol pol2pol seg del org ss n ent elst)

  ;; Retourne la liste dxf de la polyligne (d'après un arc ou un cercle)
  (defun arc2pol
	 (elst seg org / closed alpha delta cen elv rad lay nlst)
    (and (= (cdr (assoc 0 elst)) "CIRCLE") (setq closed T))
    (setq alpha	(if closed
		  (* pi 2)
		  (cdr (assoc 51 elst))
		)
	  delta	(if closed
		  (/ alpha seg)
		  (/ (ang<2pi (- alpha (cdr (assoc 50 elst)))) seg)
		)
	  cen	(cdr (assoc 10 elst))
	  elv	(caddr cen)
	  cen	(list (car cen) (cadr cen))
	  rad	(cdr (assoc 40 elst))
	  lay	(if org
		  (assoc 8 elst)
		  (cons 8 (getvar "CLAYER"))
		)
	  nlst	(vl-remove-if-not
		  (function (lambda (x) (member (car x) '(210 -3))))
		  elst
		)
	  nlst	(cons (cons 10 (polar cen alpha rad)) nlst)
    )
    (repeat (if	closed
	      (1- seg)
	      seg
	    )
      (setq
	nlst (cons (cons 10
			 (polar cen (setq alpha (- alpha delta)) rad)
		   )
		   nlst
	     )
      )
    )
    (setq nlst
	   (cons '(0 . "LWPOLYLINE")
		 (cons '(100 . "AcDbEntity")
		       (cons (cons 410 (getvar "CTAB"))
			     (cons lay
				   (cons '(100 . "AcDbPolyline")
					 (cons (cons 90
						     (if closed
						       seg
						       (1+ seg)
						     )
					       )
					       (cons (cons 70
							   (if closed
							     1
							     0
							   )
						     )
						     (cons (cons 38 elv) nlst)
					       )
					 )
				   )
			     )
		       )
		 )
	   )
    )
  )


  ;; Retourne la liste dxf de la polyligne modifiée (d'après une polyligne)

  (defun pol2pol (elst	seg   org   /	  cnt	closed	    nlst  p0
		  p1	p2    bu    larg  inc	bdata delta cen	  rad
		  alpha	n
		 )
    (setq closed (logand 1 (cdr (assoc 70 elst)))
	  cnt	 0
    )
    (and (= closed 1) (setq p0 (cdr (assoc 10 elst))))
    (while elst
      (if (= (caar elst) 10)
	(progn
	  (setq	p1 (cdar elst)
		p2 (cdr (assoc 10 (cdr elst)))
		bu (cdr (assoc 42 elst))
	  )
	  (if (or (= 0 bu)
		  (and (zerop closed) (null p2))
	      )
	    (setq nlst (cons (cadddr elst)
			     (cons (caddr elst)
				   (cons (cadr elst)
					 (cons (car elst) nlst)
				   )
			     )
		       )
		  elst (cddddr elst)
	    )
	    (progn
	      (and (not p2) (= closed 1) (setq p2 p0))
	      (setq larg  (cdr (assoc 40 elst))
		    inc	  (/ (- (cdr (assoc 41 elst)) larg) seg)
		    bdata (BulgeData bu p1 p2)
		    delta (/ (car bdata) seg)
		    rad	  (abs (cadr bdata))
		    cen	  (caddr bdata)
		    alpha (angle cen p1)
		    n	  0
		    cnt	  (+ cnt seg -1)
	      )
	      (while (< n seg)
		(setq nlst (cons
			     (cons 10
				   (polar cen
					  (+ alpha (* delta n))
					  rad
				   )
			     )
			     nlst
			   )
		      nlst (cons (cons 40 larg) nlst)
		      nlst (cons (cons 41 (setq larg (+ larg inc))) nlst)
		      nlst (cons '(42 . 0.0) nlst)
		      n	   (1+ n)
		)
	      )
	      (setq elst (cddddr elst))
	    )
	  )
	)
	(setq nlst (cons (car elst) nlst)
	      elst (cdr elst)
	)
      )
    )
    (or	org
	(setq nlst (subst (cons 8 (getvar "CLAYER")) (assoc 8 nlst) nlst))
    )
    ((lambda (dxf90)
       (subst (cons 90 (+ (cdr dxf90) cnt))
	      dxf90
	      (reverse (subst '(42 . 0.0) (assoc 42 nlst) nlst))
       )
     )
      (assoc 90 nlst)
    )
  )

  ;; Fonction principale

  (or (getenv "SegmentsNumberPerCircle")
      (setenv "SegmentsNumberPerCircle" "64")
  )
  (initget 6)
  (if 

;;;;;;;;;; French version ;;;;;;;;;; 
;;    (setq seg (getint
;;		(strcat	"\nNombre de segments par arc <"
;;			(getenv "SegmentsNumberPerCircle")
;;			">: "
;;		)
;;	      )
;;    )

;;;;;;;;;; US/English version ;;;;;;;;;; 
    (setq seg (getint
		(strcat	"\nNumber of Segments per Arc <"
			(getenv "SegmentsNumberPerCircle")
			">: "
		)
	      )
    ) 
;;;;;;;;;; US/English version ;;;;;;;;;; 


     (setenv "SegmentsNumberPerCircle" (itoa seg))
     (setq seg (atoi (getenv "SegmentsNumberPerCircle")))
  ) 


;;;;;;;;;; French version ;;;;;;;;;; 
;;  (initget "Oui Non")
;;  (if (= "Oui"
;;	 (getkword "\nEffacer les objets source [Oui/Non] ? <N>: ")
;;      )
;;    (setq del T)
;;  )

;;;;;;;;;; US/English version ;;;;;;;;;; 
  (initget "Yes No")
  (if (= "Yes"
	 (getkword "\nErase Source Objects [Yes/No] ? <N>: ")
      )
    (setq del T)
  )
;;;;;;;;;; US/English version ;;;;;;;;;; 

;;;;;;;;;; French version ;;;;;;;;;;
;;  (initget "Courant Origine")
;;  (if (= "Origine"
;;	 (getkword
;;	   "\nCalque des nouveaux objets [Courant/Origine] ? <C>: "
;;	 )
;;      )
;;    (setq org T)
;;  ) 

;;;;;;;;;; US/English version ;;;;;;;;;; 
  (initget "Current Original")
  (if (= "Original"
	 (getkword
	   "\nLayer for NEW Objects [Current/Original] ? <C>: "
	 )
      )
    (setq org T)
  ) 
;;;;;;;;;; US/English version ;;;;;;;;;; 

;;;;;;;;;; French version ;;;;;;;;;;
;;  (prompt
;;    "\nSélectionner les objets à traiter ou <tous>."
;;  ) 

;;;;;;;;;; US/English version ;;;;;;;;;; 
  (prompt
    "\nSelect Objects or <all>."
  ) 




  (and
    (or	(setq ss (ssget '((0 . "ARC,CIRCLE,LWPOLYLINE"))))
	(setq ss (ssget "_X" '((0 . "ARC,CIRCLE,LWPOLYLINE"))))
    )
    (setq n 0)
    (while (setq ent (ssname ss n))
      (setq elst (entget ent '("*")))
      (if (= (cdr (assoc 0 elst)) "LWPOLYLINE")
	((if del
	   entmod
	   entmake
	 )	   (pol2pol elst seg org)
	)
	(progn
	  (entmake (arc2pol elst seg org))
	  (and del (entdel ent))
	)
      )
      (setq n (1+ n))
    )
  )
  (princ)
)


;; BulgeData
;; Retourne les données d'un polyarc (angle rayon centre) 

(defun BulgeData (bu p1 p2 / alpha rad cen)
  (setq	alpha (* 2 (atan bu))
	rad   (/ (distance p1 p2)
		 (* 2 (sin alpha))
	      )
	cen   (polar p1
		     (+ (angle p1 p2) (- (/ pi 2) alpha))
		     rad
	      )
  )
  (list (* alpha 2.0) rad cen)
)

;;; Ang<2pi
;;; Retourne l'angle, à 2*k*pi près, compris entre 0 et 2*pi

(defun ang<2pi (ang)
  (if (and (<= 0 ang) (< ang (* 2 pi)))
    ang
    (ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))
  )
) 
 
