; 中鼎工程建造部
; 功能說明:
;	圖層動畫
;   可用來規劃建造4D/吊裝模擬/運輸模擬
;
; v0.1 - 2018/2/21 - 梁瑞元
; 	基本功能
;   依照圖層編號由1號圖層開始逐一顯示至使用者自訂輸入之N號
;   圖層名稱必須為1,2,3,4,5,6,7,8,9,10,11,12,13,14...N
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