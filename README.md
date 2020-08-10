# Head- and Eye-based Features for Continuous Core Affect Prediction

This repository contains feature extraction scripts for the above titled research.

## Useful software tools/packages/functions for use with (or to gather) the features of this repo
OpenFace (latest version [link](https://github.com/TadasBaltrusaitis/OpenFace)) <br />
openSMILE (2.3.0)		Note: speech feature extraction for use with eye and head features<br />
CURRENNT (0.2 rc1)		Note: deep neural network training and prediction<br />
OpenCV (3.4.0)			Note: OpenFace depencency<br />
mplayer (1.1-4.8)		Note: mp4 to wav file conversion for openSMILE<br />
R package, DescTools (0.99.21)	Note: CCC analysis<br />
R package, infotheo (1.2.0)	Note: mutual information estimation<br />
R package, e1071 (1.6.8)	Note: skewness and kurtosis calculations<br />
R package, ncdf4 (1.16)		Note: netcdf file writing for CURRENNT input<br />
R package, foreign (0.8.69)	Note: arff file reading from openSMILE output<br />

## Attribution

You are kindly asked to reference the following papers should you use the features proposed in this work for your own research:<br />

If you use features from Eye-based_Continuous_Affect_Prediction, please cite J. O’Dwyer, N. Murray, and R. Flynn, “Eye-based Continuous Affect Prediction,”
in 2019 8th International Conference on Affective Computing and Intelligent Interaction (ACII), IEEE, Sep. 2019, pp. 137–143, ISBN: 978-1-7281-3888-6.  

If you use features from Speech_Head_and_Eye-based_Cues_for_Continuous_Affect_Prediction, please cite J. O’Dwyer, “Speech, Head, and Eye-based Cues for Continuous Affect Prediction,”
in 2019 8th International Conference on Affective Computing and Intelligent Interaction Workshops and Demos (ACIIW), IEEE, Sep. 2019, pp. 16–20, ISBN: 978-1-7281-3891-6

## License

This project is licensed under the Responsible AI Source Code License - see the [LICENSE](LICENSE) file for details
