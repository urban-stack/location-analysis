# Summary of Streetview literature

* Thermal environment (ENVI-met)
    * Thermal environment simulation
		 (Rayman, Land surface temperature(LST) using ordinary least squares and geo-weighted regression)
    * Thermal comfort simulation 
		(vegetation cover affect wind speed and humidity, Rayman- simulate thermal comfort)
    * Solar radiation
		(Project solar trajectory map onto fisheye images from the street view- thermal environment, the lighting of buildings,  plant growth, pedestrian health, solar glare time for car driving)
*  Neighborhood morphology
    * Land use type
		(Remote sensing data, LiDAR data, Street View Images, support vector machine(SVM)- land use classification)
    * Street canyon morphology
		(calculate street aspect ratio(height/width), street view factor, street symmetry, street orientation, street alignment, canyon type(intersection, viaducts, acoustic barriers) - determin solar radiation received in street canyon, wind spped, rate of pollutant dispersion)
    * Building and façade
		(form, color, materials: tagged building images with architectural styles, deformable part-based models(DPM), multinomial Latent Logistic Regression(MLLR)- architectural style recognition, retrive fine boundaries of individual buildings)
*  Environmental perception of neighborhood
	(integration of street view with socio-economic data like government statistics, human perception data like questionnaires and crowdworker audits)
    * Street walkability
		(Street View Images- streets quality and morphology - combined with social media check-ins, POIs, Gis analysis , factors influencing Street Walkanility(SW) - assessing and predicting SW, Visual crowdworkers audits, questionnaires - collect people's perception data of SW)
		(1 crowdworkers audits- assess small sample of streets, 2 street view images- quantify landscape characteristics and analyses the impact on street walkability like street greenness, street enclosure, people's walking activity, 3 image processing techniques for street view image- count number of people and vehicles)
		(Overpass-turbo- road vector data, heading = start degree+0, 90, 180, 270, Pixellib trained on ADE20K)
    * Spatial emotional perception
		(visual quality of the street like beauty, quietness and pleasantness, and degree of urban management affect psychological state of residents, street crime rates)
    * Noise perception
		(building facades, courtyards, streetscapes affect noise annoyance and stress values towards noise)
    * Vitality and health perception
		(green view index affect people's willingness  to do activities, security affect people's physical and mental health)
* Analysis of socio-economic factors
    * Population distribution
		(population density related to intensity of urban construction and degree of street greening)
		(street view image + data of material attributes of urban space like motor vehicle brand, education level - analysis and prediction of population, economy and health, crime rates, house prices)
    * Lifestyle
		(people's activities and lifestyles of the inhabitants)
* Landscape design and environmental assessment
    * Street greening
		(green view index(GVI) based street greening study, Sky view factor(SVF) based street greening study, street trees visual audits)
    * Quality of space
		(objective spatial quality and subjective psychological perception of users)
    * Environmental characteristics
((historic) street view image- extract architectural style, building façade like windows, doors, balcony lights, morphology of street wall (HOG method for historical google street view images))
