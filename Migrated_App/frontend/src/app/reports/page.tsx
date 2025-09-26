'use client'

import { useState, useEffect } from 'react'
import { 
  ChartBarIcon,
  DocumentTextIcon,
  PrinterIcon,
  CalendarIcon,
  CurrencyDollarIcon,
  UsersIcon,
  TruckIcon,
  CubeIcon,
  ArrowDownTrayIcon,
  ShoppingCartIcon,
  CalculatorIcon
} from '@heroicons/react/24/outline'
import { Card, StatsCard } from '@/components/UI/Card'
import Button from '@/components/UI/Button'
import PageHeader from '@/components/Layout/PageHeader'

interface ReportCategory {
  id: string
  name: string
  description: string
  icon: string
  reports: Report[]
  reportCount: number
}

interface Report {
  id: string
  name: string
  description: string
  lastGenerated?: string
  frequency: string
  format: string[]
  category: string
}

export default function ReportsPage() {
  const [loading, setLoading] = useState(true)
  const [selectedCategory, setSelectedCategory] = useState<string>('financial')
  const [reportCategories, setReportCategories] = useState<ReportCategory[]>([])
  const [generating, setGenerating] = useState<string | null>(null)

  useEffect(() => {
    const fetchReportsData = async () => {
      try {
        const response = await fetch('http://localhost:8000/api/v1/reports/summary')
        if (response.ok) {
          const data = await response.json()
          setReportCategories(data.categories || [])
        }
      } catch (error) {
        console.error('Failed to fetch reports data:', error)
      } finally {
        setLoading(false)
      }
    }

    fetchReportsData()
  }, [])

  const iconMap: Record<string, any> = {
    'ChartBarIcon': ChartBarIcon,
    'ShoppingCartIcon': UsersIcon,
    'TruckIcon': TruckIcon,
    'CubeIcon': CubeIcon,
    'CalculatorIcon': CurrencyDollarIcon
  }

  const generateReport = async (reportId: string, format: string) => {
    setGenerating(reportId)
    try {
      const response = await fetch(`http://localhost:8000/api/v1/reports/generate/${reportId}?format=${format}`)
      if (response.ok) {
        const data = await response.json()
        alert(data.message || `Report generated! Download ${reportId}.${format}`)
      }
    } catch (error) {
      console.error('Failed to generate report:', error)
      alert('Failed to generate report')
    } finally {
      setGenerating(null)
    }
  }

  const defaultReportCategories: ReportCategory[] = [
    {
      name: 'Financial Reports',
      description: 'P&L, Balance Sheet, Trial Balance, and financial statements',
      icon: CurrencyDollarIcon,
      color: 'bg-green-500',
      reports: [
        {
          id: 'trial_balance',
          name: 'Trial Balance',
          description: 'Complete trial balance with all GL accounts',
          last_run: '2024-01-15T09:30:00Z',
          frequency: 'Daily',
          format: ['PDF', 'Excel', 'CSV'],
          parameters: ['Period', 'Date Range', 'Level of Detail']
        },
        {
          id: 'profit_loss',
          name: 'Profit & Loss Statement',
          description: 'Income statement showing revenue and expenses',
          last_run: '2024-01-15T08:45:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['Period', 'Comparative', 'Budget Comparison']
        },
        {
          id: 'balance_sheet',
          name: 'Balance Sheet',
          description: 'Statement of financial position',
          last_run: '2024-01-14T17:20:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['As At Date', 'Comparative', 'Consolidation']
        },
        {
          id: 'cash_flow',
          name: 'Cash Flow Statement',
          description: 'Statement of cash flows by operating, investing, and financing activities',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['Period', 'Method (Direct/Indirect)']
        }
      ]
    },
    {
      name: 'Sales Reports',
      description: 'Customer analysis, aging reports, and sales performance',
      icon: UsersIcon,
      color: 'bg-blue-500',
      reports: [
        {
          id: 'customer_aging',
          name: 'Customer Aging Report',
          description: 'Outstanding receivables by aging buckets',
          last_run: '2024-01-15T10:15:00Z',
          frequency: 'Weekly',
          format: ['PDF', 'Excel', 'CSV'],
          parameters: ['As At Date', 'Customer Range', 'Currency']
        },
        {
          id: 'sales_analysis',
          name: 'Sales Analysis',
          description: 'Sales performance by customer, product, and territory',
          last_run: '2024-01-15T07:30:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['Period', 'Group By', 'Show Comparatives']
        },
        {
          id: 'customer_statements',
          name: 'Customer Statements',
          description: 'Monthly statements for all customers',
          last_run: '2024-01-01T12:00:00Z',
          frequency: 'Monthly',
          format: ['PDF'],
          parameters: ['Statement Date', 'Customer Range', 'Show Payments']
        }
      ]
    },
    {
      name: 'Purchase Reports',
      description: 'Supplier analysis, AP aging, and purchase performance',
      icon: TruckIcon,
      color: 'bg-purple-500',
      reports: [
        {
          id: 'supplier_aging',
          name: 'Supplier Aging Report',
          description: 'Outstanding payables by aging buckets',
          last_run: '2024-01-15T09:45:00Z',
          frequency: 'Weekly',
          format: ['PDF', 'Excel', 'CSV'],
          parameters: ['As At Date', 'Supplier Range', 'Currency']
        },
        {
          id: 'purchase_analysis',
          name: 'Purchase Analysis',
          description: 'Purchase performance and spend analysis',
          last_run: '2024-01-14T16:20:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['Period', 'Supplier', 'Category']
        },
        {
          id: 'po_status',
          name: 'Purchase Order Status',
          description: 'Status of all purchase orders and receipts',
          frequency: 'As Required',
          format: ['PDF', 'Excel'],
          parameters: ['Date Range', 'Status', 'Supplier']
        }
      ]
    },
    {
      name: 'Stock Reports',
      description: 'Inventory valuation, movement reports, and stock analysis',
      icon: CubeIcon,
      color: 'bg-orange-500',
      reports: [
        {
          id: 'stock_valuation',
          name: 'Stock Valuation Report',
          description: 'Inventory valuation by location and category',
          last_run: '2024-01-15T08:00:00Z',
          frequency: 'Daily',
          format: ['PDF', 'Excel', 'CSV'],
          parameters: ['As At Date', 'Location', 'Category', 'Costing Method']
        },
        {
          id: 'abc_analysis',
          name: 'ABC Analysis',
          description: 'Inventory classification by value and usage',
          last_run: '2024-01-10T14:30:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['Analysis Period', 'Classification Basis']
        },
        {
          id: 'slow_moving',
          name: 'Slow Moving Stock',
          description: 'Items with low turnover and obsolete stock',
          last_run: '2024-01-12T11:15:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          parameters: ['Days Threshold', 'Min Value', 'Location']
        }
      ]
    }
  ]

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <PrinterIcon className="h-4 w-4" />
        Print Queue
      </Button>
      <Button variant="outline" size="sm">
        <CalendarIcon className="h-4 w-4" />
        Schedule Report
      </Button>
      <Button size="sm">
        <DocumentTextIcon className="h-4 w-4" />
        Custom Report
      </Button>
    </div>
  )

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-GB', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    })
  }

  const selectedCategoryData = reportCategories.find(cat => 
    cat.name.toLowerCase().includes(selectedCategory)
  ) || reportCategories[0]

  return (
    <div className="min-h-screen bg-gray-50">
      <PageHeader
        title="Reports"
        description="Financial reports, analysis, and business intelligence"
        actions={quickActions}
        breadcrumbs={[
          { label: 'Dashboard', href: '/' },
          { label: 'Reports' }
        ]}
      />

      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Summary Stats */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <StatsCard
            title="Total Reports"
            value="25+"
            icon={<DocumentTextIcon className="h-6 w-6" />}
            href="/reports/all"
          />
          <StatsCard
            title="Scheduled Reports"
            value="12"
            icon={<CalendarIcon className="h-6 w-6" />}
            href="/reports/scheduled"
          />
          <StatsCard
            title="Reports Run Today"
            value="8"
            icon={<ChartBarIcon className="h-6 w-6" />}
            href="/reports/history"
          />
          <StatsCard
            title="Custom Reports"
            value="3"
            icon={<PrinterIcon className="h-6 w-6" />}
            href="/reports/custom"
          />
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-4 gap-8">
          {/* Report Categories */}
          <div className="lg:col-span-1">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <h3 className="text-lg font-medium text-gray-900">Report Categories</h3>
              </div>
              <div className="p-0">
                <nav className="space-y-1">
                  {reportCategories.map((category) => {
                    const isSelected = category.name.toLowerCase().includes(selectedCategory)
                    return (
                      <button
                        key={category.name}
                        onClick={() => setSelectedCategory(category.name.toLowerCase().split(' ')[0])}
                        className={`w-full text-left px-6 py-3 flex items-center space-x-3 hover:bg-gray-50 transition-colors ${
                          isSelected ? 'bg-indigo-50 border-r-2 border-indigo-500' : ''
                        }`}
                      >
                        <div className={`p-2 rounded-md ${category.color} text-white`}>
                          <category.icon className="h-4 w-4" />
                        </div>
                        <div className="flex-1">
                          <p className={`text-sm font-medium ${
                            isSelected ? 'text-indigo-900' : 'text-gray-900'
                          }`}>
                            {category.name}
                          </p>
                          <p className="text-xs text-gray-500">
                            {category.reports.length} reports
                          </p>
                        </div>
                      </button>
                    )
                  })}
                </nav>
              </div>
            </Card>
          </div>

          {/* Reports List */}
          <div className="lg:col-span-3">
            <Card>
              <div className="px-6 py-4 border-b border-gray-200">
                <div className="flex items-center justify-between">
                  <div>
                    <h3 className="text-lg font-medium text-gray-900">
                      {selectedCategoryData?.name || 'Reports'}
                    </h3>
                    <p className="text-sm text-gray-500">
                      {selectedCategoryData?.description || 'Select a category to view reports'}
                    </p>
                  </div>
                  <Button variant="outline" size="sm">
                    <ArrowDownTrayIcon className="h-4 w-4" />
                    Export All
                  </Button>
                </div>
              </div>
              <div className="p-0">
                {loading ? (
                  <div className="p-6">
                    <div className="animate-pulse space-y-4">
                      {Array.from({ length: 4 }).map((_, index) => (
                        <div key={index} className="border border-gray-200 rounded-lg p-4">
                          <div className="flex items-start justify-between">
                            <div className="flex-1 space-y-2">
                              <div className="h-4 bg-gray-300 rounded w-3/4"></div>
                              <div className="h-3 bg-gray-300 rounded w-full"></div>
                              <div className="h-3 bg-gray-300 rounded w-1/2"></div>
                            </div>
                            <div className="ml-4">
                              <div className="h-8 w-20 bg-gray-300 rounded"></div>
                            </div>
                          </div>
                        </div>
                      ))}
                    </div>
                  </div>
                ) : (
                  <div className="p-6 space-y-4">
                    {(selectedCategoryData?.reports || []).map((report) => (
                      <div key={report.id} className="border border-gray-200 rounded-lg p-4 hover:shadow-md transition-shadow">
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <h4 className="text-base font-medium text-gray-900 mb-1">
                              {report.name}
                            </h4>
                            <p className="text-sm text-gray-600 mb-2">
                              {report.description}
                            </p>
                            <div className="flex items-center space-x-4 text-xs text-gray-500">
                              <span>Frequency: {report.frequency}</span>
                              {report.last_run && (
                                <span>Last run: {formatDate(report.last_run)}</span>
                              )}
                              <span>Formats: {report.format.join(', ')}</span>
                            </div>
                            {report.parameters && (
                              <div className="mt-2">
                                <div className="flex flex-wrap gap-1">
                                  {report.parameters.map((param, index) => (
                                    <span
                                      key={index}
                                      className="inline-flex items-center px-2 py-1 rounded-md text-xs font-medium bg-gray-100 text-gray-800"
                                    >
                                      {param}
                                    </span>
                                  ))}
                                </div>
                              </div>
                            )}
                          </div>
                          <div className="ml-4 flex flex-col space-y-2">
                            {report.format.map((fmt) => (
                              <Button
                                key={fmt}
                                size="sm"
                                variant={fmt === report.format[0] ? 'default' : 'outline'}
                                onClick={() => generateReport(report.id, fmt.toLowerCase())}
                                disabled={generating === report.id}
                              >
                                <ArrowDownTrayIcon className="h-4 w-4" />
                                {generating === report.id ? 'Generating...' : fmt}
                              </Button>
                            ))}
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
            </Card>
          </div>
        </div>

        {/* Recent Reports */}
        <div className="mt-8">
          <Card>
            <div className="px-6 py-4 border-b border-gray-200">
              <div className="flex items-center justify-between">
                <h3 className="text-lg font-medium text-gray-900">Recently Generated Reports</h3>
                <Button variant="outline" size="sm">
                  View All
                </Button>
              </div>
            </div>
            <div className="p-0">
              <div className="divide-y divide-gray-200">
                {reportCategories.length === 0 ? (
                  <div className="px-6 py-12 text-center text-gray-500">
                    <p>No recent reports available</p>
                  </div>
                ) : (
                  <div className="px-6 py-12 text-center text-gray-500">
                    <p>No recently generated reports</p>
                  </div>
                )}
              </div>
            </div>
          </Card>
        </div>
      </main>
    </div>
  )
}