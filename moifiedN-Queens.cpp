#include  <iostream>
#include  <fstream>
#include  <cstring>
#include  <cstdlib>
#include  <cmath>
#include  <queue>
#include  <random>
class set{
public:
  int x,y;
  set(){
  }
  set(int x,int y){
    this->x=x;
    this->y=y;
  }
};
class node{ // base structure for storing nodes.
public:
    char **zoo;
    static int sze;
    int x,y,cur;
    node(node *n){
      this->zoo = new char*[sze];
      for (int i=0;i<sze;i++)
        this->zoo[i]=new char[sze];
      for (int i=0;i<sze;i++)
        strncpy(this->zoo[i],n->zoo[i],sze);
    }
    node(char** zoo){
      this->zoo=zoo;
      cur=x=y=-1;
    }
    node(int x,int y,int curr,char**zoo){
      this->zoo = new char*[sze];
      for (int i=0;i<sze;i++)
        this->zoo[i]=new char[sze];
      for (int i=0;i<sze;i++)
        strncpy(this->zoo[i],zoo[i],sze);
      this->x=x;
      this->y=jump(x,y);
      this->cur=curr+1;
      this->zoo[x][y]='1';
    }
    int jump(int i,int j){
        for(;j<this->sze;j++)
          if(this->zoo[i][j]=='2')
            return j+1;
        return this->sze;
    }
};
int node::sze=0;
void printsol( bool,char ** ,int );
bool heuristic( int ,int **,int ,int ,int );
bool notinvalid(node *,int ,int );
int** generateHeuristic(node* ,int );
//DFS Algorithm Begin
class DFS{
public:
  node *n;
  int sze,liz;
  bool status;
  int **tree;
  DFS(node *n,int sze,int liz){
    status=false;
    this->n=n;
    this->sze=sze;
    this->liz=liz;
    tree=generateHeuristic(n,sze);
   }
  void find(int no,int x,int y){
    if(status)
      return;
    if(no==liz){
      status=true;
      return;
    }
    for(int i=x;i<sze;i++)
      for(int j=(i==x)?y:0;j<sze;j++)
        if(notinvalid(n,i,j)){
          if((liz-no)>tree[i][j])
          return;
          n->zoo[i][j]='1';
          find(no+1,i,this->n->jump(i,j+1));
          if(status)return;
          n->zoo[i][j]='0';
      }
    }
};
//DFS Algorithm End
//BFS Algorithm Begin
class BFS{
public:
  node *n;
  int sze,liz;
  bool status;
  int **tree;
  BFS(node *n,int sze,int liz){
    status=false;
    this->n=n;
    this->sze=sze;
    this->liz=liz;
    this->n->x=0;
    this->n->y=0;
    this->n->cur=0;
    tree=generateHeuristic(n,sze);
    }
  void find(){
    std::queue<node*> q;
    q.push(this->n);
    while(!q.empty()){
      node* tmp=NULL;
      tmp=q.front();
      if(tmp->cur==liz){
        status=true;
        n=tmp;
        return;
      }
      q.pop();
      for(int i=tmp->x;i<sze;i++)
        for(int j=(i==tmp->x)?tmp->y:0;j<sze;j++)
          if(notinvalid(tmp,i,j)){
            if((liz-tmp->cur)>tree[i][j]){
              i=sze;
              break;
            }
            q.push(new node(i,j,tmp->cur,tmp->zoo));
          }
      for (int i=0;i<sze;i++)
        delete[] tmp->zoo[i];
      delete[] tmp->zoo;
      delete tmp;
    }
  }
};
//BFS Algorithm End
//SA Algorithm Begin
class SA{
public:
  node* n;
  int size,nliz;
  bool status;
  int etime,energy;
  double temp,decay,freeze;
  set *s;
  SA(node *n,int size,int nliz,int etime,double temp,double decay, double freeze){
    this->n=n;
    this->size=size;
    this->nliz=nliz;
    status=false;
    s=new set[nliz];
    this->etime=time(0)+etime;
    this->temp=temp;
    this->decay=decay;
    this->freeze=freeze;
  }
  void find(){
    init();
	if(etime>=time(0))
		return;
    this->energy=numConflicts(n);
    while(etime>=time(0)&&temp>freeze){
      if(this->energy==0){
        status=true;
        return;
      }
      newSolution();
      this->temp-=decay; // change decay rate function
    }
  }
  void init(){
    std::random_device rd;
    std::default_random_engine generator(rd());
    std::uniform_int_distribution<int> distribution(0,size-1);
    for (int i=0;i<nliz;i++){
      again:
	if(etime>=time(0))
		return;
      int x=distribution(generator);
      int y=distribution(generator);
      if(n->zoo[x][y]!='0'){
        goto again;
      }
      s[i].x=x;
      s[i].y=y;
      n->zoo[x][y]='1';
    }
  }
  int  numConflicts(node*n){
      int c=0;
      for (int i=0;i<nliz;i++){
        int x=s[i].x;
        int y=s[i].y;
        for(int j=x+1;j<size&&n->zoo[j][y]!='2';j++)
          if(n->zoo[j][y]=='1')
            c++;
        for(int j=y+1;j<size&&n->zoo[x][j]!='2';j++)
          if(n->zoo[x][j]=='1')
            c++;
        for(int a=x+1,b=y+1;a<size&&b<size&&n->zoo[a][b]!='2';a++,b++)
          if(n->zoo[a][b]=='1')
            c++;
        for(int a=x+1,b=y-1;a<size&&b>=0&&n->zoo[a][b]!='2';a++,b--)
          if(n->zoo[a][b]=='1')
            c++;
      }
      return c;
    }
  bool acceptability(int energy,double temp){
    if(energy>=0)
      return true;
    std::random_device random;
    std::mt19937 gen(random());
    std::uniform_real_distribution<> dis(0,1);
    if(dis(gen)<(-1/log(energy/temp)))
      return true;
    return false;
  }
  int newSolution(){
    std::random_device rd;
    std::default_random_engine generator(rd());
    std::uniform_int_distribution<int> dist1(0,nliz-1);
    std::uniform_int_distribution<int> dist2(0,size-1);
    int q=dist1(generator);
    stamp:
    int x=dist2(generator);
    int y=dist2(generator);
    if(n->zoo[x][y]!='0')
      goto stamp;
    n->zoo[x][y]='1';
    n->zoo[s[q].x][s[q].y]='0';
    int t1=s[q].x;
    int t2=s[q].y;
    s[q].x=x;
    s[q].y=y;
    int tmp=numConflicts(n);
    if(acceptability(this->energy-tmp,temp)){
      this->energy=tmp;
    }
    else{
      n->zoo[x][y]='0';
      s[q].x=t1;
      s[q].y=t2;
      n->zoo[s[q].x][s[q].y]='1';
    }
  }
};
// SA Algo End
int** generateHeuristic(node* n,int sze){
  int **h;
  h=new int*[sze];
  for(int i=0;i<sze;i++)
    h[i]=new int[sze];
  int tmp=0,temp=0,t1=0,t2=0;
  for(int i=sze-1;i>=0;i--)
      for(int j=sze-1;j>=0;j--){
        if(n->zoo[i][j]=='2')
          h[i][j]=0;
        else if(n->zoo[i][j]=='0'){
          if(i+1==sze||n->zoo[i+1][j]=='2')
            tmp++;
          h[i][j]=tmp;
          if(j+1==sze||n->zoo[i][j+1]=='2')
            temp++;
          h[i][j]=(temp<h[i][j])?temp:h[i][j];
          if(j+1==sze||i+1==sze||n->zoo[i+1][j+1]=='2')
            t1++;
          h[i][j]=(t1<h[i][j])?t1:h[i][j];
          if(j-1==-1||i+1==sze||n->zoo[i+1][j-1]=='2')
            t2++;
          h[i][j]=(t2<h[i][j])?t2:h[i][j];
        }
      }
 return h;
}
bool notinvalid(node *n,int i,int j){
  if(n->zoo[i][j]=='2')
    return false;
  int a=i,b=j;
    for (;a>=0&&n->zoo[a][b]!='2';a--)
      if(n->zoo[a][b]=='1')
        return false;
    for (a=i;b>=0&&n->zoo[a][b]!='2';b--)
      if(n->zoo[a][b]=='1')
        return false;
    for(a=i,b=j;a>=0&&b>=0&&n->zoo[a][b]!='2';a--,b--)
      if(n->zoo[a][b]=='1')
        return false;
    for(a=i,b=j;a>=0&&b<n->sze&&n->zoo[a][b]!='2';a--,b++)
      if(n->zoo[a][b]=='1')
        return false;
  return true;
}
void printsol( bool status,char ** zoo,int n){ // printing the final o/p to file
 std::ofstream opt;
 opt.open ("output.txt");
 if (status)
 opt<<"OK";
 else{
   opt<<"FAIL";
   return;
 }
 if (status)
 for (int i=0;i<n;i++){
   opt<<std::endl;
   for (int j=0;j<n;j++){
    opt<<zoo[i][j];
   }
 }
  opt.close();
}
//main function
int main(void) {
  std::ifstream inpt;
  inpt.open("input.txt");
  if (!inpt) {
       std::cout << "file Read Failed";
       return 1;
   }
  char type[4],num[10],num1[10];
  int size,nliz;
  inpt>>type>>num>>num1;
  size=atoi(num);
  nliz=atoi(num1);
  node::sze=size;
  char tmp1[size+1];
  char **zoo = new char*[size];
  for (int i=0;i<size;i++)
    zoo[i]=new char[size];
  for (int i=0;i<size;i++){
    inpt>>tmp1;
    strncpy(zoo[i],tmp1,size);
  }
  node *n=new node(zoo);
  printsol(false,NULL,0);
  inpt.close();
  if (type[0]=='D'){
    DFS d(n,size,nliz);
    d.find(0,0,0);
   printsol(d.status,d.n->zoo,size);
  }
  else if (type[0]=='B'){
    BFS b(n,size,nliz);
    b.find();
    if(b.status)
      printsol(true,b.n->zoo,size);
    else
      printsol(false,NULL,0);
  }
  else if (type[0]=='S'){
    SA s(n,size,nliz,280/*total run time*/,130/*temperature*/,0.00001/*decay rate*/,0/*freezing temperature*/);
    s.find();
    printsol(s.status,s.n->zoo,size);
  }
  else
    std::cout<<"Type Identification Error";//*/
  return 0;
}


