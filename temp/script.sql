CREATE DATABASE ODIN;
 
USE ODIN;
  
 CREATE TABLE CHARACTERS (
	character_id 	VARCHAR(16) NOT NULL PRIMARY KEY , 
	character_name 	VARCHAR(20) COMMENT '캐릭터이름',
   	slotinfo 	VARCHAR(100) COMMENT '슬롯 정보 : skill_id , skill_id , ....', 
    	create_time 	TIMESTAMP DEFAULT CURRENT_TIMESTAMP  COMMENT '캐릭터 생성일자'
 );
 
 -- 임시 데이터 삽입
 INSERT INTO CHARACTERS (character_id , character_name) VALUES( 'A12345', 'Tivas');
 
 -- Exception Error 테이블
 CREATE TABLE SP_EXCEPTION_TABLE (
	idx 		INT NOT NULL AUTO_INCREMENT PRIMARY KEY , 
	sp_name 	VARCHAR(100) COMMENT '프로시저 이름', 
    	msg 		TEXT COMMENT 'EXECEPTION 텍스트 메시지 ', 
    	sql_state 	VARCHAR(5) COMMENT 'EXCEPTION SQL STATE CODE',
    	parameters 	TEXT COMMENT 'EXCEPTION 당시의 파라미터' ,
    	reg_date 	TIMESTAMP DEFAULT CURRENT_TIMESTAMP COMMENT '등록 시간'
);
     
     
     
 DROP PROCEDURE IF EXISTS SET_SKILL_SLOT;
 
 DELIMITER ;; 
 
 CREATE PROCEDURE SET_SKILL_SLOT (
 IN  i_character_id VARCHAR(16) ,
 IN  i_slot_info VARCHAR(100),
 OUT o_return int 
 )
 BEGIN
 
	DECLARE v_error_no 		INT DEFAULT 0;
    	DECLARE v_message 		TEXT;
	DECLARE v_sql_state 		VARCHAR(5);
       
   	DECLARE v_return		INT DEFAULT 0;
    
    	DECLARE CONTINUE HANDLER FOR SQLEXCEPTION 
    	BEGIN
		GET DIAGNOSTICS CONDITION 1 
        	v_message = MESSAGE_TEXT ,
        	v_sql_state = RETURNED_SQLSTATE;
        
        	SET v_error_no = -99;
   
    	END;
    
	SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED;

	RETURN_ERROR :
	BEGIN

		IF NOT EXISTS(SELECT 1 FROM CHARACTERS WHERE character_id = i_character_id)
		THEN
			SET v_return = -1;
			LEAVE RETURN_ERROR;
		END IF;

		START TRANSACTION;

			UPDATE CHARACTERS SET slotinfo = i_slot_info WHERE character_id = i_character_id;
		    	IF v_error_no < 0 
		    	THEN 
				ROLLBACK;
				SET v_return = -2;
				LEAVE RETURN_ERROR;
		    	END IF;
			
			
			-- 수행 될 내용 추가 
			-- ....
			 -- ....

		COMMIT;


	END;
        
    
    	IF v_error_no = -99
    	THEN   
		-- 파라미터 저장은 상황에 따라 변경 처리 
		INSERT INTO SP_EXCEPTION_TABLE(sp_name , msg , sql_state , parameters) VALUES('SET_SKILL_SLOT' , v_message , v_sql_state , i_slot_info);
    	END IF;
    
    
	SET o_return =  v_return;
 
 END;;
 
 DELIMITER ;
 
 
 
 CALL SET_SKILL_SLOT('A12345' , '1001,1002,1003,1004,1005,1006,1007,1008', @o_return);
 SELECT @o_return;
 
 
 SELECT character_id , character_name , slotinfo FROM CHARACTERS;
 
 
