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
		   "\nselect base point of equipment"
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
;;;	 (print itm)
	 (setq points (append points (list itm)))

;;;	 (setq dstp itm)
;;;	 (if (not srcp) 
;;;	 	(setq srcp itm)
;;;	 )
;;;
;;;	 (setq dist_between_routine (distance srcp dstp))
;;;	 (setq length_of_equipment (distance equipment_front equipment_rear))
;;;
;;;	 (setq angleofeqp (rtd (angle equipment_front equipment_rear)))
;;;	 (setq angleofpath (rtd (angle srcp dstp)))
;;;	 (setq angleofrotate (- (+ angleofpath 180) angleofeqp))
;;;
;;;	 (command "rotate" equipment "" equipment_base angleofrotate)
;;;	 (command "copy" equipment "" equipment_base dstp)
;;;	 (command "rotate" equipment "" equipment_base (- 0 angleofrotate))
;;;
;;;	 (setq srcp dstp)
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