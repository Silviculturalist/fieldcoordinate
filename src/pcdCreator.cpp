#include <Rcpp.h>
#include <iostream>
#include <fstream>
using namespace Rcpp;
using namespace std;



//This functions takes a data.frame with columns 'x','y','z' and writes to a pcd file.
//[[Rcpp::export]]
int writeToPointCloud(Rcpp::DataFrame x, std::string filename)
  {

  //check which coordinates to write.
  bool xCoordBool = x.containsElementNamed("x");
  bool yCoordBool = x.containsElementNamed("y");
  bool zCoordBool = x.containsElementNamed("z");

  //Stop if missing x or y coordinates. Needs to be at least 2D!
  if(!(xCoordBool && yCoordBool)){
    stop("Must contain at least the columns 'x', and 'y'.");
  }


  //get rowcount
  int WIDTH = x.nrows();

  // Increase type vector length by number of coordinate planes (x..y..z)
  //Concatenate TYPE and COUNT and SIZE
  std::string TYPE = "F";

  for(int i=0; i < (yCoordBool+xCoordBool+zCoordBool-1);i++){
    TYPE = TYPE.append(" F");
  }

  std::string SIZE = "4";

  for(int i=0; i < (yCoordBool+xCoordBool+zCoordBool-1);i++){
    SIZE = SIZE.append(" 4");
  }

  std::string COUNT = "1";

  for(int i=0; i < (yCoordBool+xCoordBool+zCoordBool-1);i++){
    COUNT = COUNT.append(" 1");
  }

  //Retrieve field names - should be same vector length as xCoordBool+yCoordBool+zCoordBool
  CharacterVector fieldnames = x.names();

  std::string FIELDS{};

  for(int i=0;i<((fieldnames.size()));i++)
    {
    if(i==0){
      FIELDS=fieldnames[0];
    } else{
      std::string spacer = " ";
      FIELDS = FIELDS.append(spacer);
      FIELDS = FIELDS.append(fieldnames[i]);
    }
    }

  //Instantiate stream class to write.
  ofstream newfile;

  //Set filename
  newfile.open(filename);

  //Writing header
  newfile << "VERSION .7\n";
  newfile << "FIELDS " + FIELDS << std::endl;
  newfile << "SIZE " + SIZE << std::endl;
  newfile << "TYPE " + TYPE << std::endl;
  newfile << "COUNT " + COUNT << std::endl;
  newfile << "WIDTH " + to_string(WIDTH) << std::endl;
  newfile << "HEIGHT 1" << std::endl;
  newfile << "VIEWPOINT 0 0 0 1 0 0 0" << std::endl;
  newfile << "POINTS " + to_string(WIDTH) << std::endl;
  newfile << "DATA ascii" << std::endl;

  for(int i=0;i<WIDTH;i++)
    {
    for(int j=0;j<fieldnames.size();j++)
      {
      if(j==(fieldnames.size()-1))
      {
        NumericVector x2 = x[j];
        newfile << to_string(x2[i]) + "\n";
      } else{
        NumericVector x2 = x[j];
        newfile << to_string(x2[i]) + " ";
      }
      }
  }


  return 0;
}

/*** R
writeToPointCloud(x = data.frame("x"=c(1:12),"y"=c(1:12),"z"=c(1:12)),filename = "text.pcd")
*/
