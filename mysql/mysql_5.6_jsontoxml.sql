DROP DATABASE IF EXISTS COMMON;
CREATE DATABASE IF NOT EXISTS COMMON;
USE COMMON;

DROP FUNCTION IF EXISTS encode_xml;
DELIMITER //
CREATE FUNCTION encode_xml(
    txt TEXT CHARSET utf8
) RETURNS text CHARSET utf8
    NO SQL
    DETERMINISTIC
    SQL SECURITY INVOKER
    COMMENT 'Encode (escape) given text for XML'
BEGIN
  SET   txt := REPLACE(txt, '&', '&amp;');
  SET   txt := REPLACE(txt, '<', '&lt;');
  SET   txt := REPLACE(txt, '>', '&gt;');
  SET   txt := REPLACE(txt, '"', '&quot;');
  SET   txt := REPLACE(txt, '''', '&apos;');
  
  RETURN txt;
END//
DELIMITER ;

DROP FUNCTION IF EXISTS json_to_xml;
DELIMITER //
CREATE FUNCTION json_to_xml(
    json_text   text charset utf8
) RETURNS text CHARSET utf8
    MODIFIES SQL DATA
    DETERMINISTIC
    SQL SECURITY INVOKER
    COMMENT 'Transforms JSON to XML'
BEGIN
    DECLARE v_from, v_old_from int unsigned;
    DECLARE v_token text;
    DECLARE v_level int;
    DECLARE v_state, expect_state varchar(255);
    DECLARE _json_tokens_id int unsigned default 0;
    DECLARE is_lvalue, is_rvalue tinyint unsigned;
    DECLARE scope_stack text charset ascii;
    DECLARE xml text charset utf8;
    DECLARE xml_nodes, xml_node text charset utf8;
    
    SET json_text := trim_wspace(json_text);
    
    SET expect_state := 'object_begin';
    SET is_lvalue := true;
    SET is_rvalue := false;
    SET scope_stack := '';
    SET xml_nodes := '';
    SET xml_node := '';
    SET xml := '';
    get_token_loop: REPEAT
        SET v_old_from = v_from;
        CALL    _get_json_token(json_text, v_from, v_level, v_token, 1, v_state);
        SET _json_tokens_id := _json_tokens_id + 1;
        IF v_state = 'whitespace' THEN
          ITERATE get_token_loop;
        END IF;
        IF v_level < 0 THEN
            RETURN NULL;
          -- call throw('Negative nesting level found in _get_json_tokens');
        END IF;
        IF v_state = 'start' AND scope_stack = '' THEN
          LEAVE get_token_loop;
        END IF;
        IF FIND_IN_SET(v_state, expect_state) = 0 THEN
            RETURN NULL;
          -- call throw(CONCAT('Expected ', expect_state, '. Got ', v_state));
        END IF;
        IF v_state = 'array_end' AND LEFT(scope_stack, 1) = 'o' THEN
            RETURN NULL;
          -- call throw(CONCAT('Missing "}". Found ', v_state));
        END IF;
        IF v_state = 'object_end' AND LEFT(scope_stack, 1) = 'a' THEN
            RETURN NULL;
          -- call throw(CONCAT('Missing "]". Found ', v_state));
        END IF;
        IF v_state = 'alpha' AND LOWER(v_token) NOT IN ('true', 'false', 'null') THEN
            RETURN NULL;
          -- call throw(CONCAT('Unsupported literal: ', v_token));
        END IF;
        SET is_rvalue := FALSE;
        CASE 
          WHEN  v_state = 'object_begin' THEN
            SET   expect_state := 'string', scope_stack := CONCAT('o', scope_stack), is_lvalue := TRUE;
          WHEN  v_state = 'array_begin' THEN
            SET expect_state := 'string,object_begin', scope_stack := CONCAT('a', scope_stack), is_lvalue := FALSE;
          WHEN  v_state = 'string' AND is_lvalue THEN 
            SET expect_state := 'colon', xml_node := v_token;
          WHEN  v_state = 'colon' THEN
            SET expect_state := 'string,number,alpha,object_begin,array_begin', is_lvalue := FALSE;
          WHEN  FIND_IN_SET(v_state, 'string,number,alpha') AND NOT is_lvalue THEN
            SET expect_state := 'comma,object_end,array_end', is_rvalue := TRUE;
          WHEN  v_state = 'object_end' THEN
            SET expect_state := 'comma,object_end,array_end', scope_stack := SUBSTRING(scope_stack, 2);
          WHEN  v_state = 'array_end' THEN
            SET expect_state := 'comma,object_end,array_end', scope_stack := SUBSTRING(scope_stack, 2);
          WHEN  v_state = 'comma' AND LEFT(scope_stack, 1) = 'o' THEN
            SET expect_state := 'string', is_lvalue := TRUE;
          WHEN  v_state = 'comma' AND LEFT(scope_stack, 1) = 'a' THEN
            SET expect_state := 'string,object_begin', is_lvalue := FALSE;
        END CASE;
        SET xml_node := unquote(xml_node);
        IF v_state = 'object_begin' THEN
          IF SUBSTRING_INDEX(xml_nodes, ',', 1) != '' THEN
            SET xml := CONCAT(xml, '<', SUBSTRING_INDEX(xml_nodes, ',', 1), '>');
          END IF;
          SET   xml_nodes := CONCAT(',', xml_nodes);
        END IF;
        IF v_state = 'string' AND is_lvalue THEN
            IF LEFT(xml_nodes, 1) = ',' THEN
                SET xml_nodes := CONCAT(xml_node, xml_nodes);
            ELSE
                SET xml_nodes := CONCAT(xml_node, SUBSTRING(xml_nodes, LOCATE(',', xml_nodes)));
            END IF;
        END IF;
        IF is_rvalue THEN
            SET xml := CONCAT(xml, '<', xml_node, '>', encode_xml(unquote(v_token)), '</', xml_node, '>');
        END IF;
        IF v_state = 'object_end' THEN
            SET xml_nodes := SUBSTRING(xml_nodes, LOCATE(',', xml_nodes) + 1);
            IF SUBSTRING_INDEX(xml_nodes, ',', 1) != '' THEN
                SET xml := concat(xml, '</', substring_index(xml_nodes, ',', 1), '>');
            END IF;
        END IF;
    until 
        v_old_from = v_from
    END REPEAT;
    RETURN xml;
END//
DELIMITER ;

DROP FUNCTION IF EXISTS trim_wspace;
DELIMITER //
CREATE FUNCTION trim_wspace(
    txt TEXT CHARSET utf8
) RETURNS text CHARSET utf8
    NO SQL
    DETERMINISTIC
    SQL SECURITY INVOKER
    COMMENT 'Trim whitespace characters on both sides'
BEGIN
  DECLARE   len     INT UNSIGNED DEFAULT 0;
  DECLARE   done    TINYINT UNSIGNED DEFAULT 0;
  IF txt IS NULL THEN
    RETURN  txt;
  END IF;
  WHILE NOT done do
    SET len := CHAR_LENGTH(txt);
    SET txt = trim(' ' FROM txt);
    SET txt = trim('\r' FROM txt);
    SET txt = trim('\n' FROM txt);
    SET txt = trim('\t' FROM txt);
    SET txt = trim('\b' FROM txt);
    IF CHAR_LENGTH(txt) = len THEN
      SET   done := 1;
    END IF;
  END WHILE;
  RETURN txt;
end//
DELIMITER ;

DROP FUNCTION IF EXISTS unquote;
DELIMITER //
CREATE FUNCTION unquote(
    txt TEXT CHARSET utf8
) RETURNS text CHARSET utf8
    NO SQL
    DETERMINISTIC
    SQL SECURITY INVOKER
    COMMENT 'Unquotes a given text'
BEGIN
    DECLARE quoting_char                  VARCHAR(1) CHARSET utf8;
    DECLARE terminating_quote_escape_char VARCHAR(1) CHARSET utf8;
    DECLARE current_pos                   INT UNSIGNED;
    DECLARE end_quote_pos                 INT UNSIGNED;

    if CHAR_LENGTH(txt) < 2 THEN
        RETURN  txt;
    END IF;
  
    SET   quoting_char := LEFT(txt, 1);
    
    IF NOT quoting_char IN ('''', '"', '`', '/') THEN
        RETURN txt;
    END IF;
    IF txt IN ('''''', '""', '``', '//') THEN
        RETURN '';
    END IF;
  
    SET   current_pos := 1;
    terminating_quote_loop: WHILE current_pos > 0 do

        SET current_pos := LOCATE(quoting_char, txt, current_pos + 1);
        IF current_pos = 0 THEN
        -- No terminating quote
            RETURN txt;
        END IF;
        
        IF SUBSTRING(txt, current_pos, 2) = REPEAT(quoting_char, 2) THEN
            SET current_pos := current_pos + 1;
            ITERATE terminating_quote_loop;
        END IF;
        
        SET terminating_quote_escape_char := SUBSTRING(txt, current_pos - 1, 1);
        IF (terminating_quote_escape_char = quoting_char) OR (terminating_quote_escape_char = '\\') THEN
        -- This isn't really a quote end: the quote is escaped. 
        -- We do nothing; just a trivial assignment.
            ITERATE terminating_quote_loop;
        END IF;
        -- Found terminating quote.
        LEAVE terminating_quote_loop;
    END WHILE;
    IF current_pos = CHAR_LENGTH(txt) THEN
        RETURN SUBSTRING(txt, 2, CHAR_LENGTH(txt) - 2);
    END IF;
    RETURN txt;
END//
DELIMITER ;

-- 프로시저 COMMON_DB._get_json_token 구조 내보내기
DROP PROCEDURE IF EXISTS _get_json_token;
DELIMITER //
CREATE PROCEDURE _get_json_token(
    in      p_text              text charset utf8
,   inout   p_from              int unsigned
,   inout   p_level             int
,   out     p_token             text charset utf8
,   in      allow_script_tokens int
,   inout   p_state             enum(
                                    'alpha'
                                ,   'alphanum'
                                ,   'colon'
                                ,   'comma'                        
                                ,   'decimal'
                                ,   'error'
                                ,   'integer'
                                ,   'number'
                                ,   'minus'
                                ,   'object_begin'
                                ,   'object_end'
                                ,   'array_begin'
                                ,   'array_end'
                                ,   'start'
                                ,   'string'
                                ,   'whitespace'
                                )               
)
    NO SQL
    DETERMINISTIC
    SQL SECURITY INVOKER
    COMMENT 'Reads a token according to lexical rules for JSON'
BEGIN    
    DECLARE v_length        int unsigned DEFAULT CHARACTER_LENGTH(p_text);
    DECLARE v_char, v_lookahead, v_quote_char    varchar(1) CHARSET utf8;
    DECLARE v_from          int unsigned;
    DECLARE negative_number bool DEFAULT FALSE;

    IF p_from IS NULL THEN
        SET p_from = 1;
    END IF;
    IF p_level IS NULL THEN
        SET p_level = 0;
    end if;
    IF p_state = 'object_end' then
        set p_level = p_level - 1;
    END IF;
    IF p_state = 'array_end' AND allow_script_tokens THEN
        SET p_level = p_level - 1;
    END IF;
    
    SET v_from = p_from;
    SET p_token = ''
    ,   p_state = 'start';
    
    my_loop: WHILE v_from <= v_length do
        SET v_char = substr(p_text, v_from, 1)
        ,   v_lookahead = substr(p_text, v_from+1, 1);
        
        IF v_char = '-' THEN
            SET negative_number := TRUE, v_from = v_from + 1;
            ITERATE my_loop;
        END IF;
        
        state_case: BEGIN CASE p_state
            WHEN 'error' THEN
                SET p_from = v_length;
                LEAVE state_case;            
            WHEN 'start' THEN
                CASE
                    WHEN v_char = '-' THEN
                        SET p_state = 'minus', v_from = v_from + 1;
                    WHEN v_char BETWEEN '0' AND '9' THEN 
                        SET p_state = 'integer';
                    WHEN v_char BETWEEN 'A' AND 'Z' 
                    OR   v_char BETWEEN 'a' AND 'z' 
                    OR   v_char = '_' THEN
                        SET p_state = 'alpha';                        
                    WHEN v_char = ' ' then 
                        SET p_state = 'whitespace'
                        ,   v_from = v_length - CHARACTER_LENGTH(LTRIM(SUBSTRING(p_text, v_from)));
                        LEAVE state_case;
                    WHEN v_char IN ('\t', '\n', '\r') THEN
                        SET p_state = 'whitespace';
                    WHEN v_char = '"' THEN
                        SET p_state = 'string', v_quote_char = v_char;
                    WHEN v_char = '.' then
                        IF substr(p_text, v_from + 1, 1) BETWEEN '0' AND '9' THEN
                            SET p_state = 'decimal', v_from = v_from + 1;
                        ELSE
                            SET p_state = 'error';
                            LEAVE my_loop;
                        END IF;
                    WHEN v_char = ',' THEN
                        SET p_state = 'comma', v_from = v_from + 1;
                        LEAVE my_loop;
                    WHEN v_char = ':' THEN 
                        SET p_state = 'colon', v_from = v_from + 1;
                        LEAVE my_loop;
                    WHEN v_char = '{' THEN 
                        SET p_state = 'object_begin', v_from = v_from + 1, p_level = p_level + 1;
                        LEAVE my_loop;
                    WHEN v_char = '}' THEN
                        SET p_state = 'object_end', v_from = v_from + 1;
                        LEAVE my_loop;
                    WHEN v_char = '[' THEN
                        SET p_state = 'array_begin', v_from = v_from + 1, p_level = p_level + 1;
                        LEAVE my_loop;
                    WHEN v_char = ']' then 
                        SET p_state = 'array_end', v_from = v_from + 1;
                        LEAVE my_loop;
                    ELSE
                        SET p_state = 'error';
                END CASE;
            WHEN 'alpha' THEN
                CASE
                    WHEN v_char BETWEEN 'A' AND 'Z' 
                    OR   v_char BETWEEN 'a' AND 'z' 
                    OR   v_char = '_' THEN
                        LEAVE state_case;
                    WHEN v_char BETWEEN '0' AND '9' THEN
                        SET p_state = 'alphanum';
                    ELSE
                        LEAVE my_loop;
                END CASE;
            WHEN 'alphanum' THEN
                CASE
                    WHEN v_char BETWEEN 'A' AND 'Z' 
                    OR   v_char BETWEEN 'a' AND 'z' 
                    OR   v_char = '_'
                    OR   v_char BETWEEN '0' and '9' then 
                        LEAVE state_case;
                    ELSE
                        LEAVE my_loop;
                END CASE;
            WHEN 'integer' THEN
                CASE 
                    WHEN v_char BETWEEN '0' AND '9' THEN
                        LEAVE state_case;
                    WHEN v_char = '.' THEN 
                        SET p_state = 'decimal';
                    ELSE
                        LEAVE my_loop;                        
                END CASE;
            WHEN 'decimal' THEN
                CASE
                    WHEN v_char BETWEEN '0' AND '9' THEN
                        LEAVE state_case;
                    ELSE
                        LEAVE my_loop;
                END CASE;
            WHEN 'whitespace' THEN
                IF v_char NOT IN ('\t', '\n', '\r') THEN
                    LEAVE my_loop;                        
                END IF;
            WHEN 'string' THEN
                SET v_from = LOCATE(v_quote_char, p_text, v_from);
                IF v_from THEN
                    IF substr(p_text, v_from + 1, 1) = v_quote_char THEN
                        SET v_from = v_from + 1;
                    ELSEIF substr(p_text, v_from - 1, 1) != '\\' THEN
                        SET v_from = v_from + 1;
                        LEAVE my_loop;
                    END IF;
                ELSE
                    SET p_state = 'error';
                    LEAVE my_loop;
                END IF;
            ELSE
                LEAVE my_loop;            
        END CASE; END state_case;
        SET v_from = v_from + 1;
    END WHILE my_loop;
    SET p_token = substr(p_text, p_from, v_from - p_from) collate utf8_general_ci;
    SET p_from = v_from;
    IF p_state IN ('decimal', 'integer') THEN
      SET   p_state := 'number';
    END IF;
    IF p_state = 'alphanum' THEN
      SET   p_state := 'alpha';
    END IF;
    IF negative_number AND (p_state != 'number') THEN
    	SET p_token := NULL;
    END IF;
END//
DELIMITER ;

