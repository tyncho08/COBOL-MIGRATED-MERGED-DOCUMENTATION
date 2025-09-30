'use client'

import { useState } from 'react'
import { 
  DocumentTextIcon,
  ArrowDownTrayIcon,
  PrinterIcon,
  CalendarIcon,
  ChartBarIcon,
  ClipboardDocumentListIcon,
  CurrencyDollarIcon,
  UsersIcon,
  ClockIcon,
  EnvelopeIcon
} from '@heroicons/react/24/outline'
import { Card } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'
import Select from '@/components/UI/Select'
import Input from '@/components/UI/Input'
import { EmailService } from '@/lib/emailService'

interface Report {
  id: string
  title: string
  description: string
  category: string
  icon: any
  lastRun?: string
  frequency?: string
}

export default function ReportsPage() {
  const [selectedPeriod, setSelectedPeriod] = useState('current_month')
  const [selectedFormat, setSelectedFormat] = useState('pdf')

  const reports: Report[] = [
    // Customer Reports
    {
      id: 'customer-list',
      title: 'Customer Master List',
      description: 'Complete list of all customers with contact details and credit limits',
      category: 'Customers',
      icon: UsersIcon,
      lastRun: '2024-03-15'
    },
    {
      id: 'customer-activity',
      title: 'Customer Activity Report',
      description: 'Sales activity by customer for selected period',
      category: 'Customers',
      icon: ChartBarIcon,
      lastRun: '2024-03-14'
    },
    {
      id: 'customer-statements',
      title: 'Customer Statements',
      description: 'Individual or batch customer account statements',
      category: 'Customers',
      icon: DocumentTextIcon,
      frequency: 'Monthly'
    },
    // Sales Reports
    {
      id: 'sales-summary',
      title: 'Sales Summary Report',
      description: 'Total sales by period, customer, product, or region',
      category: 'Sales',
      icon: CurrencyDollarIcon,
      lastRun: '2024-03-15'
    },
    {
      id: 'invoice-register',
      title: 'Sales Invoice Register',
      description: 'List of all invoices for selected period',
      category: 'Sales',
      icon: ClipboardDocumentListIcon,
      lastRun: '2024-03-15'
    },
    {
      id: 'sales-analysis',
      title: 'Sales Analysis Report',
      description: 'Detailed sales analysis with comparisons and trends',
      category: 'Sales',
      icon: ChartBarIcon,
      frequency: 'Weekly'
    },
    // Receivables Reports
    {
      id: 'aged-receivables',
      title: 'Aged Receivables Report',
      description: 'Outstanding invoices aging analysis by period buckets',
      category: 'Receivables',
      icon: ClockIcon,
      lastRun: '2024-03-15',
      frequency: 'Weekly'
    },
    {
      id: 'overdue-accounts',
      title: 'Overdue Accounts Report',
      description: 'List of overdue customer accounts requiring attention',
      category: 'Receivables',
      icon: ClockIcon,
      lastRun: '2024-03-14'
    },
    {
      id: 'credit-limit',
      title: 'Credit Limit Report',
      description: 'Customers approaching or exceeding credit limits',
      category: 'Receivables',
      icon: CurrencyDollarIcon,
      lastRun: '2024-03-13'
    },
    // Payment Reports
    {
      id: 'payment-history',
      title: 'Payment History Report',
      description: 'Customer payment history and patterns',
      category: 'Payments',
      icon: CurrencyDollarIcon,
      lastRun: '2024-03-15'
    },
    {
      id: 'cash-receipts',
      title: 'Cash Receipts Journal',
      description: 'Daily cash receipts summary',
      category: 'Payments',
      icon: ClipboardDocumentListIcon,
      frequency: 'Daily'
    }
  ]

  const reportCategories = [...new Set(reports.map(r => r.category))]

  const handleRunReport = (reportId: string) => {
    alert(`Running report: ${reportId} for period: ${selectedPeriod} in ${selectedFormat} format`)
  }

  const handleScheduleReport = (reportId: string) => {
    alert(`Scheduling report: ${reportId}`)
  }

  const handleEmailReport = (reportId: string) => {
    const report = reports.find(r => r.id === reportId)
    if (!report) return
    
    const recipientEmail = prompt('Enter recipient email address:', 'recipient@example.com')
    
    if (recipientEmail) {
      const format = selectedFormat.toUpperCase()
      EmailService.sendReport(report.title, recipientEmail, format)
    }
  }

  const periodOptions = [
    { value: 'today', label: 'Today' },
    { value: 'yesterday', label: 'Yesterday' },
    { value: 'current_week', label: 'Current Week' },
    { value: 'last_week', label: 'Last Week' },
    { value: 'current_month', label: 'Current Month' },
    { value: 'last_month', label: 'Last Month' },
    { value: 'current_quarter', label: 'Current Quarter' },
    { value: 'last_quarter', label: 'Last Quarter' },
    { value: 'current_year', label: 'Current Year' },
    { value: 'last_year', label: 'Last Year' },
    { value: 'custom', label: 'Custom Range' }
  ]

  const formatOptions = [
    { value: 'pdf', label: 'PDF' },
    { value: 'excel', label: 'Excel' },
    { value: 'csv', label: 'CSV' },
    { value: 'preview', label: 'Screen Preview' }
  ]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Sales Reports"
        description="Generate and manage sales ledger reports"
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Sales', href: '/sales' },
          { label: 'Reports' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Report Options */}
        <Card className="mb-8">
          <div className="p-6">
            <h3 className="text-lg font-medium text-gray-900 mb-4">Report Options</h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <Select
                label="Period"
                value={selectedPeriod}
                onChange={(e) => setSelectedPeriod(e.target.value)}
                options={periodOptions}
              />
              <Select
                label="Output Format"
                value={selectedFormat}
                onChange={(e) => setSelectedFormat(e.target.value)}
                options={formatOptions}
              />
              <Input
                label="Email To (optional)"
                type="email"
                placeholder="email@example.com"
              />
            </div>
          </div>
        </Card>

        {/* Reports by Category */}
        {reportCategories.map(category => (
          <div key={category} className="mb-8">
            <h2 className="text-lg font-semibold text-gray-900 mb-4">{category}</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              {reports
                .filter(report => report.category === category)
                .map(report => {
                  const Icon = report.icon
                  return (
                    <Card key={report.id} className="hover:shadow-lg transition-shadow">
                      <div className="p-6">
                        <div className="flex items-start justify-between mb-4">
                          <div className="flex items-center">
                            <div className="flex-shrink-0 p-3 bg-blue-50 rounded-lg">
                              <Icon className="h-6 w-6 text-blue-600" />
                            </div>
                            <div className="ml-4">
                              <h3 className="text-lg font-medium text-gray-900">{report.title}</h3>
                              <p className="text-sm text-gray-500 mt-1">{report.description}</p>
                            </div>
                          </div>
                        </div>
                        
                        <div className="mt-4 space-y-2">
                          {report.lastRun && (
                            <div className="text-sm text-gray-500">
                              Last run: {report.lastRun}
                            </div>
                          )}
                          {report.frequency && (
                            <div className="text-sm text-gray-500">
                              Frequency: {report.frequency}
                            </div>
                          )}
                        </div>

                        <div className="mt-6 flex space-x-2">
                          <Button
                            size="sm"
                            onClick={() => handleRunReport(report.id)}
                          >
                            <PrinterIcon className="h-4 w-4 mr-1" />
                            Run
                          </Button>
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={() => handleScheduleReport(report.id)}
                          >
                            <CalendarIcon className="h-4 w-4 mr-1" />
                            Schedule
                          </Button>
                          <Button
                            variant="outline"
                            size="sm"
                            onClick={() => handleEmailReport(report.id)}
                          >
                            <EnvelopeIcon className="h-4 w-4 mr-1" />
                            Email
                          </Button>
                        </div>
                      </div>
                    </Card>
                  )
                })}
            </div>
          </div>
        ))}

        {/* Quick Reports */}
        <Card className="mt-8">
          <div className="p-6">
            <h3 className="text-lg font-medium text-gray-900 mb-4">Quick Reports</h3>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <Button variant="outline" className="justify-start">
                <DocumentTextIcon className="h-4 w-4 mr-2" />
                Today's Sales
              </Button>
              <Button variant="outline" className="justify-start">
                <UsersIcon className="h-4 w-4 mr-2" />
                New Customers
              </Button>
              <Button variant="outline" className="justify-start">
                <ClockIcon className="h-4 w-4 mr-2" />
                Overdue List
              </Button>
              <Button variant="outline" className="justify-start">
                <ArrowDownTrayIcon className="h-4 w-4 mr-2" />
                Export All
              </Button>
            </div>
          </div>
        </Card>
      </main>
    </div>
  )
}