; �����u�{�سy��
; �\�໡��:
;	�ϼh�ʵe
;   �i�ΨӳW���سy4D/�Q�˼���/�B�����
;
; v0.1 - 2018/2/21 - ��礸
; 	�򥻥\��
;   �̷ӹϼh�s����1���ϼh�}�l�v�@��ܦܨϥΪ̦ۭq��J��N��
;   �ϼh�W�٥�����1,2,3,4,5,6,7,8,9,10,11,12,13,14...N
(defun c:layeranimation ()
	(setq layercount (getint "How many layers to be animated"))
  	(command "-layer" "set" "0" "")
  	(setq layerindex 1)
	(while (<= layerindex layercount)
	  	(princ layerindex)
	  	(command "-layer" "off" layerindex "")
	  	(command "-layer" "freeze" layerindex "")
		(setq layerindex (+ layerindex 1))
	)
  	(redraw)
  	(setq layerindex 1)
	(while (<= layerindex layercount)
	  	(command "-layer" "on" layerindex "")
	  	(command "-layer" "thaw" layerindex "")
	  	(command "DELAY" 1000)
	  	(redraw)
		(setq layerindex (+ layerindex 1))
	)  
)