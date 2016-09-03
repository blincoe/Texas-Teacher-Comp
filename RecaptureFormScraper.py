import requests, csv
from bs4 import BeautifulSoup

source_dir = 'C:/Users/blincoeshirey/Documents/Ian/Analysis/Texas-Teacher-Comp'

DistrictList_path = source_dir + '/Data/DistrictList.py'
OutFile_path = source_dir + '/Data/UpdatedRecapture.csv'

execfile(DistrictList_path)

YearList = ['2015', '2016', '2017']

url = 'https://tea4avfawcett.tea.state.tx.us/Fsp/Reports/ReportSelection.aspx'

InitialRequest = requests.get(url)
InitialSoup = BeautifulSoup(InitialRequest.text)


InitialViewState = InitialSoup.findAll('input', {'type': 'hidden', 'name': '__VIEWSTATE'})
InitialEventValidation = InitialSoup.findAll('input', {'type': 'hidden', 'name': '__EVENTVALIDATION'})

SelectedReportDatalist = {'__EVENTVALIDATION':InitialEventValidation[0]['value'],
            '__VIEWSTATE':InitialViewState[0]['value'],
            '__VIEWSTATEENCRYPTED': '',
            'ctl00$Body$ReportTypeDropDownList': 'CostOfRecapture',
            'ctl00$Body$SelectButton': 'Select'
}

SelectedReportRequest = requests.post(url, data = SelectedReportDatalist)
SelectedReportSoup = BeautifulSoup(SelectedReportRequest.text)

SelectedReportViewState = SelectedReportSoup.findAll('input', {'type': 'hidden', 'name': '__VIEWSTATE'})
SelectedReportEventValidation = SelectedReportSoup.findAll('input', {'type': 'hidden', 'name': '__EVENTVALIDATION'})


f = open(OutFile_path, 'wb')
writer = csv.writer(f)

# Header
writer.writerow (['DistrictID', 'SchoolYear', 'ReportDate', 'PaymentCycle', 'RecaptureAmount'])

for District in DistrictList:
    for Year in YearList:
        FinalDataList = {'__EVENTVALIDATION':SelectedReportEventValidation[0]['value'],
                     '__VIEWSTATE':SelectedReportViewState[0]['value'],
                     '__VIEWSTATEENCRYPTED':'',
                     'ctl00$Body$SchoolYearDropDownList':Year,
                     'ctl00$Body$DistrictIdTextBox':District,
                     'ctl00$Body$SubmitButton':'Submit'
        }
        FinalRequest = requests.post(url, data = FinalDataList)
        FinalSoup = BeautifulSoup(FinalRequest.text)
        table = FinalSoup.find('table', attrs={'class':'gridView'})
        if table is not None:
            try:
                LastRowIndex = len(table.find_all('tr')[1:])
                LastRow = table.find_all('tr')[LastRowIndex]
                col = LastRow.find_all('td')
                ReportDate = col[1].string
                PaymentCycle = col[2].string
                RecaptureAmount = col[4].string
                DistrictID = int(District[District.find('(') + 1: -1])
                SchoolYear = str(int(Year) - 1) + '-' + Year[2:]
                writer.writerow ([DistrictID,SchoolYear,ReportDate,PaymentCycle,RecaptureAmount])
                print 'Wrote data for ' + District + ' ' + Year
            except:
                print 'No info for ' + District + ' ' + Year
        else:
            print 'No info for ' + District + ' ' + Year

f.close()


