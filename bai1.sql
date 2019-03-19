var as_date = "201901"
SELECT  :as_date AS  WK_YYMM, DEPT_CD,  STG_CD, COUNT(*) CNT_BLK,
					 SUM(CASE WHEN REQ_BLK_YN = 'Y' THEN 1 ELSE 0 END) Q_APPRO,
					 SUM(CASE WHEN GMGR_CNF_YN = 'Y' THEN 1 ELSE 0 END) GMGR_CNF,
					 MAX(FB_UNCFM) FB_UNCFM,
					 MAX(FB_ACCEPT) FB_ACCEPT,
					 MAX(FB_ACCEPT_MH) FB_ACCEPT_MH,
					 MAX(FB_UNACPT) FB_UNACPT,
					 MAX(FB_UNACPT_MH) FB_UNACPT_MH,
					 MAX(QAS_DT)

				FROM (
						SELECT  DEPT_CD, STG_CD, BLOCK, SHIP_NO,QAS_DT,
								  CASE WHEN (SELECT COUNT(*) FROM TBLPE_DT_BLK_COMP_MSTR
																	 WHERE SHIP_NO = A.SHIP_NO
																		AND STG_CD  = A.STG_CD
																		AND BLOCK   = A.BLOCK
																		AND TRUNC(REG_DT) IS NOT NULL )> 0 THEN 'Y' ELSE 'N' END REQ_BLK_YN,
									CASE WHEN (SELECT COUNT(*) FROM TBLPE_DT_BLK_COMP_MSTR
																	 WHERE SHIP_NO = A.SHIP_NO
																		AND STG_CD  = A.STG_CD
																		AND BLOCK   = A.BLOCK
																		AND TRUNC(CNF_3RD_DT) IS NOT NULL )> 0 THEN 'Y' ELSE 'N' END GMGR_CNF_YN,
								  (SELECT NVL(SUM(CNT),0)
									  FROM
										  ( SELECT T2.MGR_NO, T2.REG_DT, T2.STG_CD , REC_DEPT, CASE WHEN ISS_CNF_DT IS NULL THEN 0 ELSE 1 END CNT
											  FROM
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS ISS_DEPT, B.CHK_MGR_CNFM_DT AS ISS_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '1'
														AND A.PRGS_STAT = 'A'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 0  ) T1,
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS REC_DEPT, B.CHK_MGR_CNFM_DT AS REC_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '1'
														AND A.PRGS_STAT = 'A'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 1 ) T2
											 WHERE T1.SHIP_NO = T2.SHIP_NO
												AND T1.MGR_NO  = T2.MGR_NO

										  )
								  WHERE STG_CD   = A.STG_CD
								  AND REC_DEPT = A.DEPT_CD
								  )  FB_UNCFM,

										(SELECT NVL(SUM(CNT),0)
									  FROM
										  ( SELECT T2.MGR_NO, T2.REG_DT, T2.STG_CD , REC_DEPT, CASE WHEN ISS_CNF_DT IS NOT NULL AND REC_CNF_DT IS NOT NULL THEN 1 ELSE 0 END CNT
											  FROM
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS ISS_DEPT, B.CHK_MGR_CNFM_DT AS ISS_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'B'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 0  ) T1,
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS REC_DEPT, B.CHK_MGR_CNFM_DT AS REC_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'B'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 1 ) T2
											 WHERE T1.SHIP_NO = T2.SHIP_NO
												AND T1.MGR_NO  = T2.MGR_NO

										  )
								  WHERE STG_CD   = A.STG_CD
								  AND REC_DEPT = A.DEPT_CD
								  )FB_ACCEPT,
										 (SELECT NVL(SUM(MH_REQ),0)
									  FROM
										  ( SELECT T2.MGR_NO, T2.REG_DT, T1.RESP_CD, T2.STG_CD , REC_DEPT, CASE WHEN ISS_CNF_DT IS NOT NULL AND REC_CNF_DT IS NOT NULL THEN T2.MH_REQ ELSE 0 END MH_REQ
											  FROM
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.MH_REQ, A.REG_DT, A.STG_CD, DEPT_CD AS ISS_DEPT, B.CHK_MGR_CNFM_DT AS ISS_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'B'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 0  ) T1,
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.MH_REQ, A.REG_DT, A.STG_CD, DEPT_CD AS REC_DEPT, B.CHK_MGR_CNFM_DT AS REC_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'B'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 1 ) T2
											 WHERE T1.SHIP_NO = T2.SHIP_NO
												AND T1.MGR_NO  = T2.MGR_NO

										  )
								  WHERE STG_CD   = A.STG_CD
								  AND REC_DEPT = A.DEPT_CD
								  ) FB_ACCEPT_MH,
								  (SELECT NVL(COUNT(*),0)
									  FROM
										  ( SELECT T2.MGR_NO, T1.RESP_CD, T2.REG_DT, T2.STG_CD , REC_DEPT
											  FROM
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS ISS_DEPT, B.CHK_MGR_CNFM_DT AS ISS_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'R'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 0  ) T1,
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS REC_DEPT, B.CHK_MGR_CNFM_DT AS REC_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'R'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 1 ) T2
											 WHERE T1.SHIP_NO = T2.SHIP_NO
												AND T1.MGR_NO  = T2.MGR_NO

										  )
								  WHERE STG_CD   = A.STG_CD
								  AND REC_DEPT = A.DEPT_CD
								  ) FB_UNACPT,
										(SELECT NVL(SUM(MH_REQ),0)
									  FROM
										  ( SELECT T2.MGR_NO, T2.REG_DT, T2.STG_CD , REC_DEPT, T2.MH_REQ
											  FROM
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.STG_CD, DEPT_CD AS ISS_DEPT, B.CHK_MGR_CNFM_DT AS ISS_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'R'
													  AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 0  ) T1,
													(SELECT A.SHIP_NO, A.MGR_NO, C.RESP_CD, A.REG_DT, A.MH_REQ, A.STG_CD, DEPT_CD AS REC_DEPT, B.CHK_MGR_CNFM_DT AS REC_CNF_DT
													  FROM MTB_FB_MAIN A,
															 MTB_FB_PRGS B,
															 PTB_STD_RESP C
													 WHERE A.SHIP_NO = B.SHIP_NO
														AND A.MGR_NO  = B.MGR_NO
														AND B.REC_MJR_RESPCD = C.RESP_CD
														AND A.FB_KIND = 'P'
														AND A.MGR_NO IS NOT NULL
														AND A.MGR_STAT = '2'
														AND A.PRGS_STAT = 'R'
														AND TO_CHAR(A.REG_DT,'YYYYMM') = :as_date
														AND SER_NO = 1 ) T2
											 WHERE T1.SHIP_NO = T2.SHIP_NO
												AND T1.MGR_NO  = T2.MGR_NO

										  )
								  WHERE STG_CD   = A.STG_CD
								  AND REC_DEPT = A.DEPT_CD
								  ) FB_UNACPT_MH

						  FROM
								  (SELECT DISTINCT '40' STG_CD, 'A1' DEPT_CD, SHIP_NO, BLOCK, JO_FI_RDT AS QAS_DT
								  FROM PTB_PRCSCD_ACTDT
								 WHERE TO_CHAR(JO_FI_RDT,'YYYYMM') = :as_date
									AND JO_FI_RDT IS NOT NULL
											  AND MFG_IND   = 'V'
								 UNION

								SELECT DISTINCT '72' STG_CD, 'C0' DEPT_CD, SHIP_NO, BLOCK , SUN_FI_RDT AS QAS_DT
								  FROM (
										SELECT DISTINCT SHIP_NO, CASE WHEN SUBSTR(BLOCK, 1, 3) = 'N11' THEN BLOCK
																				 WHEN SUBSTR(P1_BLK, 1, 1) IN ('1', '5') THEN P1_BLK
																				 ELSE BLOCK
																		  END BLOCK , SUN_FI_RDT
												  FROM PTB_PRCSCD_ACTDT
												 WHERE TO_CHAR(SUN_FI_RDT,'YYYYMM') = :as_date
													AND SUN_FI_RDT IS NOT NULL
													AND MFG_IND   = 'V'
										  )
								  UNION

								SELECT DISTINCT '80' STG_CD, 'A0' DEPT_CD, SHIP_NO, BLOCK, PE1_FI_RDT AS QAS_DT
								  FROM (
										SELECT DISTINCT SHIP_NO, CASE WHEN SUBSTR(BLOCK, 1, 3) = 'N11' THEN BLOCK
																				 WHEN SUBSTR(P1_BLK, 1, 1) IN ('1', '5') THEN P1_BLK
																				 ELSE BLOCK
																		  END BLOCK, PE1_FI_RDT
												  FROM PTB_PRCSCD_ACTDT
												 WHERE TO_CHAR(PE1_FI_RDT,'YYYYMM') = :as_date
													AND PE1_FI_RDT IS NOT NULL
													AND MFG_IND   = 'V'
										  )

										UNION

										SELECT DISTINCT '82' STG_CD, 'C0' DEPT_CD, SHIP_NO, BLOCK, PE1_FI_RDT AS QAS_DT
										  FROM (
													SELECT DISTINCT A.SHIP_NO, CASE WHEN SUBSTR(a.BLOCK, 1, 3) = 'N11' THEN A.BLOCK
																							 WHEN SUBSTR(A.P1_BLK, 1, 1) IN ('1', '5') THEN A.P1_BLK
																							 ELSE A.BLOCK
																					  END BLOCK, PE1_FI_RDT
															  FROM PTB_PRCSCD_ACTDT A, PTB_SHIP_MACT B
															 WHERE TO_CHAR(PE1_FI_RDT,'YYYYMM') = :as_date
																AND PE1_FI_RDT IS NOT NULL
																AND a.MFG_IND   = 'V'
																AND A.SHIP_NO = B.SHIP_NO
																AND B.STG_CD = '82'
																AND WK_KIND = 'I' AND EDIT_BDGTMH > 0 AND B.MFG_IND = 'V'
																AND CASE WHEN SUBSTR(a.BLOCK, 1, 3) = 'N11' THEN A.BLOCK
																											 WHEN SUBSTR(A.P1_BLK, 1, 1) IN ('1', '5') THEN A.P1_BLK
																											 ELSE A.BLOCK
																									  END = B.BLOCK
													)
								 UNION
									SELECT '90' STG_CD, 'A0' DEPT_CD, SHIP_NO, ERECT_BLK AS BLOCK , MAX(PE2_FI_RDT) AS QAS_DT
								  FROM
										((SELECT SHIP_NO, ERECT_BLK, P1_BLK, BLOCK, PE2_ST_RDT, PE2_FI_RDT, CASE WHEN PE2_FI_RDT IS NOT NULL THEN 'Y' ELSE 'N' END CHK_YN
										  FROM PTB_PRCSCD_ACTDT A
										 WHERE MFG_IND   = 'V'
											AND BLOCK NOT IN (SELECT BLOCK FROM GMH_MEGA_BLK_LIST B WHERE A.BLOCK = B.BLOCK AND A.SHIP_NO = B.SHIP_NO)

										UNION ALL
										SELECT R1.SHIP_NO, R1.ERECT_BLK, R1.P1_BLK, R1.BLOCK,
												 MIN(CASE WHEN R4.S_KIND = 'HCT' AND CASE WHEN ASCII(SUBSTR(R1.BLOCK, 1,1)) > 64 THEN SUBSTR(R1.BLOCK, 1,1) ELSE SUBSTR(R1.BLOCK, 2,1) END IN ('T', 'L') AND R1.BLOCK <> 'T11S' THEN R2.PE1_ST_RDT ELSE R2.PE2_ST_RDT END) AS WKSTDT,
												 MAX(CASE WHEN R4.S_KIND = 'HCT' AND CASE WHEN ASCII(SUBSTR(R1.BLOCK, 1,1)) > 64 THEN SUBSTR(R1.BLOCK, 1,1) ELSE SUBSTR(R1.BLOCK, 2,1) END IN ('T', 'L') AND R1.BLOCK <> 'T11S' THEN R2.PE1_FI_RDT ELSE R2.PE2_FI_RDT END) AS WKFIDT,
												 CASE WHEN MAX(CASE WHEN R4.S_KIND = 'HCT' AND CASE WHEN ASCII(SUBSTR(R1.BLOCK, 1,1)) > 64 THEN SUBSTR(R1.BLOCK, 1,1) ELSE SUBSTR(R1.BLOCK, 2,1) END IN ('T', 'L') AND R1.BLOCK <> 'T11S' THEN R2.PE1_FI_RDT ELSE R2.PE2_FI_RDT END) IS NOT NULL
														THEN 'Y' ELSE 'N' END CHK_YN
												 FROM  PTB_SHIP_MACT R1,
														 (SELECT A.SHIP_NO, A.ERECT_BLK, A.P1_BLK AS P1_BLK,
																 MIN(A.PE2_ST_RDT)  AS PE2_ST_RDT,  MAX(A.PE2_FI_RDT)  AS PE2_FI_RDT,
																 MIN(A.PE2_ST_PLDT) AS PE2_ST_PLDT, MAX(A.PE2_FI_PLDT) AS PE2_FI_PLDT,
																 MIN(A.PE1_ST_RDT)  AS PE1_ST_RDT,  MAX(A.PE1_FI_RDT)  AS PE1_FI_RDT,
																 MIN(A.PE1_ST_PLDT) AS PE1_ST_PLDT, MAX(A.PE1_FI_PLDT) AS PE1_FI_PLDT
														  FROM PTB_PRCSCD_ACTDT A, GMH_MEGA_BLK_LIST B
														 WHERE A.SHIP_NO = B.SHIP_NO AND A.BLOCK  = B.BLOCK AND DIV_LEV1 = '1L'
														 GROUP BY A.SHIP_NO, A.ERECT_BLK, A.P1_BLK
														 HAVING MIN(NVL(A.PE2_ST_RDT, TO_DATE('19000101', 'YYYYMMDD'))) > TO_DATE('19000101', 'YYYYMMDD') AND
																  MAX(NVL(A.PE2_FI_RDT, TO_DATE('19000101', 'YYYYMMDD'))) = TO_DATE('19000101', 'YYYYMMDD')
													  ) R2,
													  ( SELECT ARG_STR_01 AS_SHIP_DIV, ARG_STR_02 AS_MFG_IND
															 FROM GMH_STD_VIEW_ARG
															WHERE IP_ADD    = SYS_CONTEXT('USERENV','IP_ADDRESS')
															  AND WINDOW_ID = 'W_20D2030'
															  AND TAB_ID    = 1
															  AND DW_ID     = '1') R3, PTB_SHIP_BASIC R4
												WHERE R1.SHIP_NO = R2.SHIP_NO
												  AND (R1.BLOCK = R2.P1_BLK)
												  AND R1.SHIP_NO = R4.SHIP_NO
												  AND R1.STG_CD = '80' AND R1.WK_KIND = 'K'
												  AND R1.MFG_IND LIKE R3.AS_MFG_IND
												  AND F_GMH_SHIP_DIV3(R3.AS_SHIP_DIV, R2.SHIP_NO) = 'Y'
												GROUP BY R1.SHIP_NO, R1.ERECT_BLK, R1.BLOCK, R1.P1_BLK ) )A
								WHERE TO_CHAR(PE2_FI_RDT,'YYYYMM') = :as_date
								GROUP BY SHIP_NO, ERECT_BLK
								HAVING MIN(CHK_YN) = 'Y'

								 UNION

								SELECT DISTINCT 'N1' STG_CD, 'A0' DEPT_CD, A.SHIP_NO, A.BLOCK , WKFIPLDT AS QAS_DT
								  FROM PTB_SHIP_MACT A, PTB_SHIP_BASIC B, PTB_AFT_MAIN_SKIND_ZONE_NEW C
								 WHERE TRIM(B.S_KIND) = TRIM(C.S_KIND)
									AND TRIM(B.S_TYPE) = TRIM(C.S_TYPE)
									AND A.WK_KIND = C.WK_KIND
									AND A.BLOCK = C.ZONE_CD
									AND A.WK_KIND = 'K'
									AND A.STG_CD = 'BM'
									AND C.ZONE_WK_GBN = '02'
									AND C.TANK_TYPE = 'A1'
									AND C.TANK_GBN IN ('B', 'A')
									AND C.ACT_NM IN ( 'NO.5 W.B.TK', 'NO.2 W.B.TK', 'NO.6 W.B.TK','NO.5 C.O.TK','NO.1 C.O.TK' )
									AND TO_CHAR(WKFIPLDT,'YYYYMM') = :as_date

								UNION
								SELECT '7N' STG_CD, 'B1' DEPT_CD, SHIP_NO, P1_BLK, MAX(TRUNC(DO_FI_RDT)) AS QAS_DT
								  FROM (
										SELECT DISTINCT P1_BLK, SHIP_NO , DO_FI_RDT
										  FROM PTB_PRCSCD_ACTDT
										 WHERE TO_CHAR(DO_FI_RDT,'YYYYMM') = :as_date
											AND DO_FI_RDT IS NOT NULL
											AND MFG_IND   = 'V'
											AND SUBSTR(P1_BLK,2,1) NOT IN ( 'H','T','L')
											)
							GROUP BY  SHIP_NO, P1_BLK

								  UNION
								  SELECT PROGRESS_CD, DEPT_CD, SHIP_NO, 'XXXX' BLOCK, ACT_DAY AS QAS_DT
								  FROM
										(SELECT B.CHAR5 DEPT_CD, B.GRP2 PROGRESS_CD,GRP2_NM, SHIP_NO, LC   + NUM1 ACT_DAY
											FROM  V70_SHIP_EVENT A,
													(SELECT * FROM ZTB_DT_CODE WHERE GRP1 = 'P0105' and CHAR2 = 'Y' and char4 = 'LC') B
										)
								 WHERE TO_CHAR(ACT_DAY,'YYYYMM') = :as_date
								 GROUP BY DEPT_CD, PROGRESS_CD, SHIP_NO, ACT_DAY
								 UNION
								  SELECT PROGRESS_CD, DEPT_CD, SHIP_NO, 'XXXX' BLOCK, ACT_DAY AS QAS_DT
								  FROM
										(SELECT B.CHAR5 DEPT_CD, B.GRP2 PROGRESS_CD, GRP2_NM, SHIP_NO, DWT  + NUM1 ACT_DAY
											FROM  V70_SHIP_EVENT A,
													(SELECT * FROM ZTB_DT_CODE WHERE GRP1 = 'P0105' and CHAR2 = 'Y' and char4 = 'DWT') B
										)
								 WHERE TO_CHAR(ACT_DAY,'YYYYMM') = :as_date
								 GROUP BY DEPT_CD, PROGRESS_CD, SHIP_NO, ACT_DAY
								 UNION
								  SELECT PROGRESS_CD, DEPT_CD, SHIP_NO, 'XXXX' BLOCK, ACT_DAY AS QAS_DT
								  FROM
										(SELECT B.CHAR5 DEPT_CD, B.GRP2 PROGRESS_CD,GRP2_NM, SHIP_NO, S_ST  + NUM1 ACT_DAY
											FROM  V70_SHIP_EVENT A,
													(SELECT * FROM ZTB_DT_CODE WHERE GRP1 = 'P0105' and CHAR2 = 'Y' and char4 = 'ST') B
										)
								 WHERE TO_CHAR(ACT_DAY,'YYYYMM') = :as_date
								 GROUP BY DEPT_CD, PROGRESS_CD, SHIP_NO, ACT_DAY
							UNION
							 SELECT PROGRESS_CD, DEPT_CD, SHIP_NO, 'XXXX' BLOCK, ACT_DAY AS QAS_DT
							  FROM
									(
									 SELECT  B.DEPT_CD, B.PROGRESS_CD, B.GRP2_NM, A.SHIP_NO, TRUNC(DL) ACT_DAY
										FROM  V70_SHIP_EVENT A,
												(SELECT DISTINCT CHAR5 DEPT_CD, GRP2 PROGRESS_CD, GRP2_NM FROM ZTB_DT_CODE WHERE GRP2 IN ('U1','U2','U3')) B
									)
							 WHERE TO_CHAR(ACT_DAY,'YYYYMM') = :as_date
							 GROUP BY DEPT_CD, PROGRESS_CD, SHIP_NO, ACT_DAY
								 )  A
						 )  K
			WHERE DEPT_CD LIKE :as_dept||'%'
		 GROUP BY DEPT_CD, STG_CD
