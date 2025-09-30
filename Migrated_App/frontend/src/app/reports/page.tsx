'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
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
import { ReportGenerator } from '@/lib/reportGenerator'

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
  const router = useRouter()
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
          // Transform API data to match component structure
          const categories = data.categories.map((cat: any) => ({
            name: cat.name,
            description: cat.description,
            icon: iconMap[cat.icon] || DocumentTextIcon,
            color: getColorByCategory(cat.id),
            reports: cat.reports.map((report: any) => ({
              id: report.id,
              name: report.name,
              description: report.description,
              last_run: report.lastGenerated,
              frequency: report.frequency,
              format: report.format.map((f: string) => f.toUpperCase()),
              parameters: getReportParameters(report.id)
            }))
          }))
          setReportCategories(categories)
        } else {
          // Use default categories if API fails
          setReportCategories(getDefaultCategories())
        }
      } catch (error) {
        console.error('Failed to fetch reports data:', error)
        // Use default categories if API fails
        setReportCategories(getDefaultCategories())
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

  const getColorByCategory = (categoryId: string) => {
    const colors: Record<string, string> = {
      'financial': 'bg-green-500',
      'sales': 'bg-blue-500',
      'purchase': 'bg-purple-500',
      'stock': 'bg-yellow-500',
      'tax': 'bg-red-500'
    }
    return colors[categoryId] || 'bg-gray-500'
  }

  const getReportParameters = (reportId: string) => {
    const parametersMap: Record<string, string[]> = {
      'trial-balance': ['Period', 'Date Range', 'Level of Detail'],
      'profit-loss': ['Period', 'Comparative', 'Budget Comparison'],
      'balance-sheet': ['As At Date', 'Comparative', 'Consolidation'],
      'cash-flow': ['Period', 'Method (Direct/Indirect)'],
      'sales-summary': ['Date Range', 'Customer Filter', 'Product Filter'],
      'aged-receivables': ['As At Date', 'Customer Range', 'Age Buckets'],
      'stock-valuation': ['As At Date', 'Location', 'Category'],
      'vat-return': ['Period', 'Box Details']
    }
    return parametersMap[reportId] || ['Date Range']
  }

  const generateReport = async (reportId: string, format: string) => {
    setGenerating(reportId)
    try {
      // First, fetch report data from backend
      const response = await fetch(`http://localhost:8000/api/v1/reports/data/${reportId}`)
      
      if (!response.ok) {
        // If no backend data available, generate sample data
        const sampleData = generateSampleData(reportId)
        await generateReportFile(reportId, sampleData, format.toLowerCase())
        return
      }

      const data = await response.json()
      await generateReportFile(reportId, data, format.toLowerCase())
      
    } catch (error) {
      console.error('Failed to generate report:', error)
      // Generate with sample data as fallback
      const sampleData = generateSampleData(reportId)
      await generateReportFile(reportId, sampleData, format.toLowerCase())
    } finally {
      setGenerating(null)
    }
  }

  const generateReportFile = async (reportId: string, data: any, format: string) => {
    try {
      switch (reportId) {
        case 'trial_balance':
          await ReportGenerator.generateTrialBalance(data, format as 'pdf' | 'excel' | 'csv')
          break
        case 'profit_loss':
          await ReportGenerator.generateProfitLoss(data, format as 'pdf' | 'excel' | 'csv')
          break
        case 'customer_aging':
          await ReportGenerator.generateCustomerAging(data, format as 'pdf' | 'excel' | 'csv')
          break
        case 'stock_valuation':
          await ReportGenerator.generateStockValuation(data, format as 'pdf' | 'excel' | 'csv')
          break
        default:
          // For other reports, use generic format
          const genericData = {
            title: reportId.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase()),
            subtitle: `Generated on ${new Date().toLocaleDateString()}`,
            generatedDate: new Date(),
            headers: data.headers || ['Column 1', 'Column 2', 'Column 3'],
            rows: data.rows || [['Sample', 'Data', 'Row']],
            summary: data.summary
          }
          
          switch (format) {
            case 'pdf':
              await ReportGenerator.generatePDF(genericData)
              break
            case 'excel':
              await ReportGenerator.generateExcel(genericData)
              break
            case 'csv':
              await ReportGenerator.generateCSV(genericData)
              break
          }
      }
    } catch (error) {
      console.error('Failed to generate report file:', error)
      alert('Failed to generate report file')
    }
  }

  const generateSampleData = (reportId: string): any => {
    switch (reportId) {
      case 'trial_balance':
        return {
          accounts: [
            { code: '1000', name: 'Cash and Bank', debit: 125000, credit: 0, balance: 125000 },
            { code: '1100', name: 'Accounts Receivable', debit: 85000, credit: 0, balance: 85000 },
            { code: '1200', name: 'Inventory', debit: 95000, credit: 0, balance: 95000 },
            { code: '2000', name: 'Accounts Payable', debit: 0, credit: 45000, balance: -45000 },
            { code: '2100', name: 'Loans Payable', debit: 0, credit: 100000, balance: -100000 },
            { code: '3000', name: 'Share Capital', debit: 0, credit: 150000, balance: -150000 },
            { code: '4000', name: 'Sales Revenue', debit: 0, credit: 250000, balance: -250000 },
            { code: '5000', name: 'Cost of Goods Sold', debit: 150000, credit: 0, balance: 150000 },
            { code: '6000', name: 'Operating Expenses', debit: 90000, credit: 0, balance: 90000 },
          ],
          totalDebits: 545000,
          totalCredits: 545000
        }
      
      case 'profit_loss':
        return {
          revenue: [
            { name: 'Product Sales', currentPeriod: 150000, ytd: 1800000, budget: 2000000, variance: -10 },
            { name: 'Service Revenue', currentPeriod: 75000, ytd: 900000, budget: 800000, variance: 12.5 },
            { name: 'Other Income', currentPeriod: 5000, ytd: 60000, budget: 50000, variance: 20 },
          ],
          totalRevenue: { current: 230000, ytd: 2760000, budget: 2850000, variance: -3.2 },
          expenses: [
            { name: 'Cost of Sales', currentPeriod: 90000, ytd: 1080000, budget: 1140000, variance: -5.3 },
            { name: 'Salaries & Wages', currentPeriod: 50000, ytd: 600000, budget: 600000, variance: 0 },
            { name: 'Rent & Utilities', currentPeriod: 15000, ytd: 180000, budget: 180000, variance: 0 },
            { name: 'Marketing', currentPeriod: 8000, ytd: 96000, budget: 120000, variance: -20 },
            { name: 'Admin Expenses', currentPeriod: 12000, ytd: 144000, budget: 150000, variance: -4 },
          ],
          totalExpenses: { current: 175000, ytd: 2100000, budget: 2190000, variance: -4.1 },
          netProfit: { current: 55000, ytd: 660000, budget: 660000, variance: 0 },
          grossMargin: 39.1,
          netMargin: 23.9,
          ebitda: 720000
        }

      case 'customer_aging':
        return {
          customers: [
            { name: 'ABC Corporation', current: 25000, days30: 15000, days60: 8000, days90: 5000, days120plus: 2000, total: 55000 },
            { name: 'XYZ Limited', current: 18000, days30: 12000, days60: 0, days90: 0, days120plus: 0, total: 30000 },
            { name: 'Tech Solutions Inc', current: 35000, days30: 0, days60: 0, days90: 0, days120plus: 0, total: 35000 },
            { name: 'Global Trading Co', current: 12000, days30: 8000, days60: 6000, days90: 4000, days120plus: 3000, total: 33000 },
            { name: 'Prime Industries', current: 22000, days30: 0, days60: 0, days90: 0, days120plus: 0, total: 22000 },
          ],
          totalOutstanding: 175000,
          totalCurrent: 112000,
          totalOverdue: 63000,
          totalOver90: 14000
        }

      case 'stock_valuation':
        return {
          items: [
            { code: 'PRD001', description: 'Widget A - Blue', location: 'Warehouse 1', quantity: 250, unitCost: 45.50, totalValue: 11375 },
            { code: 'PRD002', description: 'Widget B - Red', location: 'Warehouse 1', quantity: 180, unitCost: 62.00, totalValue: 11160 },
            { code: 'PRD003', description: 'Component X', location: 'Warehouse 2', quantity: 500, unitCost: 12.75, totalValue: 6375 },
            { code: 'PRD004', description: 'Assembly Kit Y', location: 'Warehouse 1', quantity: 75, unitCost: 125.00, totalValue: 9375 },
            { code: 'PRD005', description: 'Spare Part Z', location: 'Warehouse 2', quantity: 1200, unitCost: 8.50, totalValue: 10200 },
          ],
          totalItems: 5,
          totalQuantity: 2205,
          totalValue: 48485,
          averageValue: 21.99
        }

      default:
        return {
          headers: ['Date', 'Description', 'Reference', 'Amount'],
          rows: [
            [new Date().toLocaleDateString(), 'Sample Transaction 1', 'REF001', '$1,250.00'],
            [new Date().toLocaleDateString(), 'Sample Transaction 2', 'REF002', '$2,500.00'],
            [new Date().toLocaleDateString(), 'Sample Transaction 3', 'REF003', '$750.00'],
          ],
          summary: [
            { label: 'Total Records', value: '3' },
            { label: 'Total Amount', value: '$4,500.00' }
          ]
        }
    }
  }

  const getDefaultCategories = (): ReportCategory[] => [
    {
      id: 'financial',
      name: 'Financial Reports',
      description: 'P&L, Balance Sheet, Trial Balance, and financial statements',
      icon: 'CurrencyDollarIcon',
      reportCount: 4,
      reports: [
        {
          id: 'trial_balance',
          name: 'Trial Balance',
          description: 'Complete trial balance with all GL accounts',
          lastGenerated: '2024-01-15T09:30:00Z',
          frequency: 'Daily',
          format: ['PDF', 'Excel', 'CSV'],
          category: 'financial'
        },
        {
          id: 'profit_loss',
          name: 'Profit & Loss Statement',
          description: 'Income statement showing revenue and expenses',
          lastGenerated: '2024-01-15T08:45:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'financial'
        },
        {
          id: 'balance_sheet',
          name: 'Balance Sheet',
          description: 'Statement of financial position',
          lastGenerated: '2024-01-14T17:20:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'financial'
        },
        {
          id: 'cash_flow',
          name: 'Cash Flow Statement',
          description: 'Statement of cash flows by operating, investing, and financing activities',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'financial'
        }
      ]
    },
    {
      id: 'sales',
      name: 'Sales Reports',
      description: 'Customer analysis, aging reports, and sales performance',
      icon: 'UsersIcon',
      reportCount: 3,
      reports: [
        {
          id: 'customer_aging',
          name: 'Customer Aging Report',
          description: 'Outstanding receivables by aging buckets',
          lastGenerated: '2024-01-15T10:15:00Z',
          frequency: 'Weekly',
          format: ['PDF', 'Excel', 'CSV'],
          category: 'sales'
        },
        {
          id: 'sales_analysis',
          name: 'Sales Analysis',
          description: 'Sales performance by customer, product, and territory',
          lastGenerated: '2024-01-15T07:30:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'sales'
        },
        {
          id: 'customer_statements',
          name: 'Customer Statements',
          description: 'Monthly statements for all customers',
          lastGenerated: '2024-01-01T12:00:00Z',
          frequency: 'Monthly',
          format: ['PDF'],
          category: 'sales'
        }
      ]
    },
    {
      id: 'purchase',
      name: 'Purchase Reports',
      description: 'Supplier analysis, AP aging, and purchase performance',
      icon: 'TruckIcon',
      reportCount: 3,
      reports: [
        {
          id: 'supplier_aging',
          name: 'Supplier Aging Report',
          description: 'Outstanding payables by aging buckets',
          lastGenerated: '2024-01-15T09:45:00Z',
          frequency: 'Weekly',
          format: ['PDF', 'Excel', 'CSV'],
          category: 'purchase'
        },
        {
          id: 'purchase_analysis',
          name: 'Purchase Analysis',
          description: 'Purchase performance and spend analysis',
          lastGenerated: '2024-01-14T16:20:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'purchase'
        },
        {
          id: 'po_status',
          name: 'Purchase Order Status',
          description: 'Status of all purchase orders and receipts',
          frequency: 'As Required',
          format: ['PDF', 'Excel'],
          category: 'purchase'
        }
      ]
    },
    {
      id: 'stock',
      name: 'Stock Reports',
      description: 'Inventory valuation, movement reports, and stock analysis',
      icon: 'CubeIcon',
      reportCount: 3,
      reports: [
        {
          id: 'stock_valuation',
          name: 'Stock Valuation Report',
          description: 'Inventory valuation by location and category',
          lastGenerated: '2024-01-15T08:00:00Z',
          frequency: 'Daily',
          format: ['PDF', 'Excel', 'CSV'],
          category: 'stock'
        },
        {
          id: 'abc_analysis',
          name: 'ABC Analysis',
          description: 'Inventory classification by value and usage',
          lastGenerated: '2024-01-10T14:30:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'stock'
        },
        {
          id: 'slow_moving',
          name: 'Slow Moving Stock',
          description: 'Items with low turnover and obsolete stock',
          lastGenerated: '2024-01-12T11:15:00Z',
          frequency: 'Monthly',
          format: ['PDF', 'Excel'],
          category: 'stock'
        }
      ]
    }
  ]

  const getReportsGeneratedToday = () => {
    if (!reportCategories.length) return 0
    const today = new Date().toDateString()
    let count = 0
    reportCategories.forEach(cat => {
      cat.reports.forEach(report => {
        if (report.lastGenerated && new Date(report.lastGenerated).toDateString() === today) {
          count++
        }
      })
    })
    return count
  }

  const getMostUsedReport = () => {
    // In a real app, this would come from usage statistics
    return "Trial Balance"
  }

  const quickActions = (
    <div className="flex space-x-2">
      <Button variant="outline" size="sm">
        <PrinterIcon className="h-4 w-4" />
        Print Queue
      </Button>
      <Button variant="outline" size="sm" onClick={() => router.push('/reports/schedule')}>
        <CalendarIcon className="h-4 w-4" />
        Schedule Report
      </Button>
      <Button size="sm" onClick={() => router.push('/reports/custom')}>
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
            value={reportCategories.reduce((sum, cat) => sum + cat.reports.length, 0).toString()}
            icon={<DocumentTextIcon className="h-6 w-6" />}
            href="/reports/all"
          />
          <StatsCard
            title="Report Categories"
            value={reportCategories.length.toString()}
            icon={<CalendarIcon className="h-6 w-6" />}
            href="/reports/categories"
          />
          <StatsCard
            title="Generated Today"
            value={getReportsGeneratedToday().toString()}
            icon={<ChartBarIcon className="h-6 w-6" />}
            href="/reports/history"
          />
          <StatsCard
            title="Most Used"
            value={getMostUsedReport()}
            icon={<PrinterIcon className="h-6 w-6" />}
            href="/reports/popular"
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
                        <div className={`p-2 rounded-md ${getColorByCategory(category.id)} text-white`}>
                          {(() => {
                            const Icon = iconMap[category.icon] || DocumentTextIcon
                            return <Icon className="h-4 w-4" />
                          })()}
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
                              {report.lastGenerated && (
                                <span>Last run: {formatDate(report.lastGenerated)}</span>
                              )}
                              <span>Formats: {report.format.join(', ')}</span>
                            </div>
                            {getReportParameters(report.id) && (
                              <div className="mt-2">
                                <div className="flex flex-wrap gap-1">
                                  {getReportParameters(report.id).map((param, index) => (
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
                <Button variant="outline" size="sm" onClick={() => router.push('/reports/history')}>
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