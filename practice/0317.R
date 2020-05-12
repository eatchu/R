########0317할일#########
# 1. 그래프 함수들 정리 (용도,내장함수,데이터읽기 등)
# 2. 파일관련 패키지+함수 정리 ㅇ
# 3. DB 연동하는법 복습 ㅇ
# 4. 새로운 데이터 구해서 전처리, 정제 
# 5. 단어구름 시각화하기 : 새로운 데이터 사용








install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/KoNLP_0.80.1.zip",repos = NULL) 
# repos=NULL의 의미
buildDictionary(ext_dic='sejong', user_dic = user_dic)
# ext_dic, user_dic
trump_cor <- Corpus(VectorSource(trump_wd))  # 벡터 소스 생성 -> 코퍼스 생성 
#corpus, vectorsource 함수의 역할
TermDocumentMatrix(myCorpusPrepro, 
                   control=list(wordLengths=c(4,16))) #2-8글자이내인것
#termdocumentmatrix
# 단어와 문장을 하나의 행렬로 만들어주는 함수
# 
# 문서 행 단어 열 DTM DocumentTermMatrix
# 단어 행 문서 열 TDM TermDocumentMatrix
# 
# 1. 대한민국은 나의 조국입니다
# 2. 나는 홍길동 입니다
# 
# 
# DTM
#       나   대한민국  조국  홍길동
# 1   1          1       1         0
# 2   1          0       0         1
# 
# TDM
#           1       2
# 나        1       1
# 대한민국  1       0
# 조국      1       0 
# 홍길동    0       1



